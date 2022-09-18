/**
* Copyright (C) 2019-2021 Xilinx, Inc
*
* Licensed under the Apache License, Version 2.0 (the "License"). You may
* not use this file except in compliance with the License. A copy of the
* License is located at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
* License for the specific language governing permissions and limitations
* under the License.
*/
#include "xcl2.hpp"

#include <experimental/xrt_device.h>
#include <experimental/xrt_kernel.h>

#include "bls12_377_g1/bls12_377_g1.h"
#include "bls12_377_g1/pippenger.h"

#include <fstream>
#include <vector>

#include <chrono>
using namespace std::chrono;

#define BITS_PER_INPUT_POINT 377*3
#define BITS_PER_OUTPUT_POINT 377*4
#define SCALAR_BITS 253
#define DDR_BITS 512

// We round up our points to the nearest multiple of the AXI stream / DDR
#define BYTES_PER_INPUT (((SCALAR_BITS + BITS_PER_INPUT_POINT + DDR_BITS - 1) / DDR_BITS) * DDR_BITS) / 8
#define BYTES_PER_OUTPUT (((BITS_PER_OUTPUT_POINT + DDR_BITS - 1) / DDR_BITS) * DDR_BITS) / 8

int test_streaming(const std::string& binaryFile, std::string& input_points)
{

    int num_points;
    int num_output_points = 90091;

    std::ifstream input_file(input_points);
    std::string line;

    // First get the number of points from the files
    for(num_points = 0; std::getline(input_file,line); num_points++);
    input_file.close();

    printf("Running MSM with [%i] input points and [%i] output points\n", num_points, num_output_points);

    auto input_size = (BYTES_PER_INPUT * num_points) / 4;
    auto output_size = (BYTES_PER_OUTPUT * num_output_points) / 4;

    cl_int err;
    cl::CommandQueue q;
    cl::Context context;
    cl::Kernel krnl_mm2s, krnl_msm_pippenger, krnl_s2mm;
    // Allocate Memory in Host Memory
    size_t vector_input_size_bytes = sizeof(int) * input_size;
    size_t vector_output_size_bytes = sizeof(int) * output_size;

    std::vector<uint32_t, aligned_allocator<uint32_t> > source_kernel_input(input_size);
    std::vector<uint32_t, aligned_allocator<uint32_t> > source_kernel_output(output_size);

    // Load input points from the test file
    input_file.open(input_points);
    if (input_file.is_open()) {
    	unsigned int point = 0;
    	while (std::getline(input_file, line)) {
            for (unsigned int i = 0; i < line.length(); i += 8) {
              std::string byteString = line.substr(line.length() - i - 8, 8);
              uint32_t word =  strtol(byteString.c_str(), NULL, 16);
              source_kernel_input[point+(i/8)] = word;
            }
            point = point + (BYTES_PER_INPUT/4);
    	}
    	input_file.close();
    }

    // OPENCL HOST CODE AREA START
    // Create Program and Kernel
    auto devices = xcl::get_xil_devices();
    auto device = devices[0];

    // read_binary_file() is a utility API which will load the binaryFile
    // and will return the pointer to file buffer.
    auto fileBuf = xcl::read_binary_file(binaryFile);
    cl::Program::Binaries bins{{fileBuf.data(), fileBuf.size()}};
    bool valid_device = false;
    for (unsigned int i = 0; i < devices.size(); i++) {
        auto device = devices[i];
        // Creating Context and Command Queue for selected Device
        OCL_CHECK(err, context = cl::Context(device, nullptr, nullptr, nullptr, &err));
        OCL_CHECK(err, q = cl::CommandQueue(context, device, CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE | CL_QUEUE_PROFILING_ENABLE, &err));

        std::cout << "Trying to program device[" << i << "]: " << device.getInfo<CL_DEVICE_NAME>() << std::endl;
        cl::Program program(context, {device}, bins, nullptr, &err);
        if (err != CL_SUCCESS) {
            std::cout << "Failed to program device[" << i << "] with xclbin file!\n";
        } else {
            std::cout << "Device[" << i << "]: program successful!\n";
            OCL_CHECK(err, krnl_mm2s = cl::Kernel(program, "krnl_mm2s", &err));
            OCL_CHECK(err, krnl_msm_pippenger = cl::Kernel(program, "krnl_msm_pippenger", &err));
            OCL_CHECK(err, krnl_s2mm = cl::Kernel(program, "krnl_s2mm", &err));
            valid_device = true;
            break; // we break because we found a valid device
        }
    }
    if (!valid_device) {
        std::cout << "Failed to program any device found, exit!\n";
        exit(EXIT_FAILURE);
    }

    // Allocate Buffer in Global Memory
    OCL_CHECK(err, cl::Buffer buffer_input(context, CL_MEM_USE_HOST_PTR | CL_MEM_READ_ONLY, vector_input_size_bytes,
                                           source_kernel_input.data(), &err));
    OCL_CHECK(err, cl::Buffer buffer_output(context, CL_MEM_USE_HOST_PTR | CL_MEM_WRITE_ONLY, vector_output_size_bytes,
                                       	   source_kernel_output.data(), &err));

    // Set the "Kernel 0" Arguments
    OCL_CHECK(err, err = krnl_mm2s.setArg(0, buffer_input));
    OCL_CHECK(err, err = krnl_mm2s.setArg(2, input_size));

    // Set the "Kernel 1" Arguments
    OCL_CHECK(err, err = krnl_s2mm.setArg(0, buffer_output));
    OCL_CHECK(err, err = krnl_s2mm.setArg(2, output_size));

    // Copy input data to device global memory
    cl::Event write_event;
    OCL_CHECK(err, err = q.enqueueMigrateMemObjects({buffer_input}, 0 /* 0 means from host*/, nullptr, &write_event));


    // Launch the writer kernel
    std::vector<cl::Event> eventVec;
    eventVec.push_back(write_event);

    // Start timer from here
    auto start = high_resolution_clock::now();

    OCL_CHECK(err, err = q.enqueueTask(krnl_mm2s, &eventVec));
    std::cout << "Launched writer kernel!" << std::endl;

    // Launch the reader kernel
    OCL_CHECK(err, err = q.enqueueTask(krnl_s2mm));
    std::cout << "Launched reader kernel!" << std::endl;

    // Wait for kernels to finish its operation
    OCL_CHECK(err, err = q.finish());

    // Stop timer here
    auto stop = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(stop - start);

    // Copy Result from Device Global Memory to Host Local Memory
    OCL_CHECK(err, err = q.enqueueMigrateMemObjects({ buffer_output}, CL_MIGRATE_MEM_OBJECT_HOST));
    OCL_CHECK(err, err = q.finish());

    // OPENCL HOST CODE AREA END

    // Convert result point
    // TODO: This should run in batches as points are streamed up.
    bls12_377_g1::init();

    bls12_377_g1::Xyzt final_result;
    bls12_377_g1::Xyzt accum, running;


    int bit_offset = 0;
    int point = 0;
    for (int window_idx = 0; window_idx < bls12_377_g1::NUM_WINDOWS; window_idx++) {
      accum.setToIdentity();
      running.setToIdentity();
      for (int bucket_idx = bls12_377_g1::NUM_BUCKETS(window_idx) - 1; bucket_idx >= 0;
           bucket_idx--) {

        point = point + (BYTES_PER_OUTPUT/4);

        uint64_t x_words[bls12_377_g1::NUM_64B_WORDS];
        uint64_t y_words[bls12_377_g1::NUM_64B_WORDS];
        uint64_t z_words[bls12_377_g1::NUM_64B_WORDS];
        uint64_t t_words[bls12_377_g1::NUM_64B_WORDS];

        for (int i = 0; i < bls12_377_g1::NUM_64B_WORDS; i++) {
          x_words[i] = source_kernel_output[2*i] + ((uint64_t)source_kernel_output[2*(i+1)] << 32);
          if (i == bls12_377_g1::NUM_64B_WORDS - 1) {
            x_words[i] = x_words[i] & ((1UL << (bls12_377_g1::NUM_BITS % 64)) - 1);
          }
        }

        bls12_377_g1::Xyzt p(x_words, y_words, z_words, t_words);

        p.postComputeFPGA();
        bls12_377_g1::triangleSumUpdate(accum, running, p);
      }
      bls12_377_g1::finalSumUpdate(final_result, accum, bit_offset);
      bit_offset += bls12_377_g1::NUM_WINDOW_BITS(window_idx);
    }

    final_result.extendedTwistedEdwardsToWeierstrass();
    final_result.println();

    std::cout << "STREAMING TEST FINISHED" << std::endl;

    std::cout << "Time taken: "
         << duration.count() << " microseconds" << std::endl;

    return 0;
}



int main(int argc, char** argv) {
    if (argc != 3) {
        std::cout << "Usage: " << argv[0] << " <XCLBIN File> <INPUT POINT File>" << std::endl;
        return EXIT_FAILURE;
    }

    int res = 0;
    std::string binaryFile = argv[1];
    std::string input_points = argv[2];
    res |= test_streaming(binaryFile, input_points);

    return res;
}
