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

#include "bls12_377_g1/bls12_377_g1.h"

#include <experimental/xrt_device.h>
#include <experimental/xrt_kernel.h>

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

int test_streaming(const std::string& binaryFile, std::string& input_points, std::string& output_points)
{
    bls12_377_g1::init();
    bls12_377_g1::print_params();

    int num_points, num_output_points;
    std::ifstream input_file(input_points);
    std::ifstream output_file(output_points);
    std::string line;

    // First get the number of points from the files
    for(num_points = 0; std::getline(input_file,line); num_points++);
    input_file.close();

    for(num_output_points = 0; std::getline(output_file,line); num_output_points++);
    output_file.close();

    printf("Running MSM with [%i] input points and [%i] output points\n", num_points, num_output_points);

    auto input_size = (BYTES_PER_INPUT * num_points) / 4;
    auto output_size = (BYTES_PER_OUTPUT * num_output_points) / 4;

    std::cout << "NUmber of input points: " << num_points << std::endl;
    std::cout << "NUmber of output points: " << num_output_points << std::endl;

    cl_int err;
    cl::CommandQueue q;
    cl::Context context;
    cl::Kernel krnl_mm2s, krnl_msm_pippenger, krnl_s2mm;
    // Allocate Memory in Host Memory
    size_t vector_input_size_bytes = sizeof(int) * input_size;
    size_t vector_output_size_bytes = sizeof(int) * output_size;

    std::vector<uint32_t, aligned_allocator<uint32_t> > source_kernel_input(input_size);
    std::vector<uint32_t, aligned_allocator<uint32_t> > source_kernel_output(output_size);
    memset(source_kernel_input.data(), 0, sizeof(uint32_t) * source_kernel_input.size());
    memset(source_kernel_output.data(), 0, sizeof(uint32_t) * source_kernel_output.size());

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
    OCL_CHECK(err, err = q.enqueueMigrateMemObjects({buffer_input}, 0 /* 0 means from host*/, nullptr));
    OCL_CHECK(err, err = q.finish());

    // Start timer from here
    auto start = high_resolution_clock::now();

    OCL_CHECK(err, err = q.enqueueTask(krnl_mm2s));
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

    // Compare out the points returned
    int failed = 0;
    output_file.open(output_points);
    if (output_file.is_open()) {
    	unsigned int point = 0;

        bls12_377_g1::Xyzt fpga;
        bls12_377_g1::Xyzt expected;

    	while (std::getline(output_file, line)) {
	    std::vector<uint32_t> line_words;

            for (unsigned int i = 0; i < line.length(); i += 8) {
              std::string byteString = line.substr(line.length() - i - 8, 8);

              line_words.push_back(strtol(byteString.c_str(), NULL, 16));
            }

            fpga.import_from_fpga_vector(source_kernel_output.data() + point);;
	    expected.import_from_fpga_vector(line_words.data());

	    // std::cout << "Point: " << point / (BYTES_PER_OUTPUT/4) << std::endl;
	    // std::cout << "  Bytes per otput" << BYTES_PER_OUTPUT << std::endl;
	    // std::cout << "FPGA:\n";
	    // fpga.println_hex();
	    // std::cout << "Expected:\n";
	    // expected.println_hex();

	    fpga.twistedEdwardsExtendedToAffine();
	    expected.twistedEdwardsExtendedToAffine();

	    bool failed_this = !(fpga == expected);

	    if (failed_this) {
		    failed = failed_this;
              std::cout << "Bucket: " << point / (BYTES_PER_OUTPUT/4) << std::endl;
	      if (failed_this) {
		      std::cout << "  Bucket failed" << std::endl;
	      }
	      std::cout << "q:\n";
	      gmp_printf("q: %#Zx\n", bls12_377_g1::q);
	      std::cout << "FPGA:\n";
	      fpga.println_hex();
	      std::cout << "Expected:\n";
	      expected.println_hex();

	      fpga.import_from_fpga_vector(source_kernel_output.data() + point, true);;
	      expected.import_from_fpga_vector(line_words.data());

	      std::cout << "FPGA (in extednded form):\n";
	      fpga.println_hex();
	      std::cout << "Expected (in extended form):\n";
	      expected.println_hex();
	    }

            point = point + (BYTES_PER_OUTPUT/4);
    	}
    	output_file.close();
    }

    std::cout << "STREAMING TEST FINISHED, ";
    if (failed == 0) {
        std::cout << "test PASSED" << std::endl;
    } else {
        std::cout << "test FAILED" << std::endl;
    }

    std::cout << "Time taken: "
         << duration.count() << " microseconds" << std::endl;

    return failed;
}



int main(int argc, char** argv) {
    if (argc != 4) {
        std::cout << "Usage: " << argv[0] << " <XCLBIN File> <INPUT POINT File> <OUTPUT POINT File>" << std::endl;
        return EXIT_FAILURE;
    }

    int res = 0;
    std::string binaryFile = argv[1];
    std::string input_points = argv[2];
    std::string output_points = argv[3];
    res |= test_streaming(binaryFile, input_points, output_points);

    return res;
}
