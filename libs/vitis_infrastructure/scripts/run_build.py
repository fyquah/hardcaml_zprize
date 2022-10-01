#!/usr/bin/python3

import argparse
import collections
import distutils.dir_util
import enum
import glob
import os
import shutil
import subprocess
import sys

parser = argparse.ArgumentParser(description="")
parser.add_argument(
        "--top-level-name",
        required=True,
        help="Name of the output xclbin vitis \'binary\'")
parser.add_argument(
        "--build-dir",
        required=True,
        help="Directory to generate build")
parser.add_argument(
        "--platform",
        required=True,
        help="Name of the vitis platform to compile for")
parser.add_argument(
        "--build-target",
        help="When secified, will actually build the given target, rather than just generating the project")
parser.add_argument(
        "--cfg-file",
        help=" Optional cfg file to be used in compilation")
parser.add_argument(
        "kernels",
        nargs="+",
        help="Directory of kernels to build")

class KernelType(enum.Enum):
    RTL = 1
    CPP = 2

Kernel = collections.namedtuple("Kernel", ["name", "type_", "has_kernel_cfg"])

def resolve_platform(platform):
    if platform == "varium-c1100":
        return "xilinx_u55n_gen3x4_xdma_2_202110_1"
    elif platform == "aws":
        return "xilinx_aws-vu9p-f1_shell-v04261818_201920_3"

    raise RuntimeError(f"Unknown paltform {platform}")

def write_makefile(args, kernels):
    assert kernels
    fname = os.path.join(args.build_dir, "Makefile")
    kernel_names = " ".join(kernel.name for kernel in kernels)
    with open(fname, "w") as f:


        f.write(f"PLATFORM={resolve_platform(args.platform)}\n")
        f.write(f"KERNEL_NAMES={kernel_names}\n")
        f.write(f"TOP_LEVEL_NAME={args.top_level_name}\n")
        if args.cfg_file is not None:
            cfg_file = os.path.basename(args.cfg_file)
            f.write(f"VPP_LDFLAGS+=--config {cfg_file}\n")
        f.write(f"include Makefile.base\n")

def write_kernels_mk(build_dir, kernels):
    fname = os.path.join(build_dir, "kernels.mk")
    # XXX fyquah: Consider supporting c++ kernels too
    with open(fname, "w") as f:
        f.write("VIVADO := $(XILINX_VIVADO)/bin/vivado\n")
        for kernel in kernels:
            kernel_name = kernel.name
            additional_vpp_flags = ""
            if kernel.has_kernel_cfg:
                additional_vpp_flags += f"--config src/{kernel_name}/kernel.cfg "

            kernel_src = f"$(TEMP_DIR)/{kernel_name}.xo: package_kernel.tcl src/{kernel_name}/kernel_ports.tcl gen_xo.tcl "

            # Only add RTL files if they exist
            if glob.glob(f"{kernel_name}/*.sv"):
                kernel_src += f"src/{kernel_name}/*.sv "
            if glob.glob(f"{kernel_name}/*.v"):
                kernel_src += f"src/{kernel_name}/*.v "

            kernel_src += "\n"

            if kernel.type_ == KernelType.RTL:
                f.write(kernel_src)
                f.write(f"\tmkdir -p $(TEMP_DIR)\n")
                f.write(f"\t$(VIVADO) -mode batch -source gen_xo.tcl -tclargs $(TEMP_DIR)/{kernel_name}.xo $(TARGET) $(PLATFORM) $(XSA) {kernel_name}\n")
            elif kernel.type_ == KernelType.CPP:
                f.write(f"$(TEMP_DIR)/{kernel_name}.xo: src/{kernel_name}/*.cpp\n")
                f.write(f"\tmkdir -p $(TEMP_DIR)\n")
                f.write(f"\tv++ $(VPP_FLAGS) {additional_vpp_flags} -c -k {kernel_name} -I'$(<D)' -o'$@' '$<'\n")
            else:
                assert False
            f.write("\n")


def copy_template_files(args):
    template_dir = os.path.join(os.path.dirname(sys.argv[0]), "..", "template")
    for f in [ "Makefile.base", "makefile_us_alveo.mk", "utils.mk", "gen_xo.tcl", "package_kernel.tcl" ]:
        src = os.path.join(template_dir, f)
        dst = os.path.join(args.build_dir, f)
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.copy(src, dst)

def copy_supporting_files(args):
    if args.cfg_file is not None:
        src = args.cfg_file
        dst = os.path.join(args.build_dir, os.path.basename(args.cfg_file))
        shutil.copy(src, dst)

    file = "pre_place.tcl"
    if os.path.exists(file):
        dst = os.path.join(args.build_dir, os.path.basename(file))
        shutil.copy(file, dst) 

    file = "pre_synth.tcl"
    if os.path.exists(file):
        dst = os.path.join(args.build_dir, os.path.basename(file))
        shutil.copy(file, dst) 

def parse_kernel_type(kernel_type: str) -> KernelType:
    try:
        return { 
                "rtl": KernelType.RTL,
                "cpp": KernelType.CPP,
                }[kernel_type.lower()]
    except KeyError:
        raise RuntimeError(f"Failed to parse kernel_type: {kernel_type}")

def copy_kernel_sources(args):
    if not args.kernels:
        raise RuntimeError("Must specify at least 1 kernel!")

    kernels = []
    for arg in args.kernels:
        splitted = arg.split(":")
        if len(splitted) == 1:
            src = splitted[0]
            kernel_type = KernelType.RTL
        elif len(splitted) == 2:
            src = splitted[0]
            kernel_type = parse_kernel_type(splitted[1])

        src = src.strip("/")
        kernel_name = os.path.basename(src)
        dst = os.path.join(args.build_dir, f"src/{kernel_name}")

        if not os.path.isdir(src):
            raise RuntimeError(f"Kernels must be a directory: {src} !")

        if kernel_type == KernelType.RTL:
            if not os.path.isfile(os.path.join(src, "kernel_ports.tcl")):
                raise RuntimeError(
                        f"Expecting RTL kernel directory to contain a kernel_ports.tcl (kernel directory = {src})")

        has_kernel_cfg = os.path.isfile(os.path.join(src, "kernel.cfg"))

        distutils.dir_util.copy_tree(src, dst)

        # Delete dune file from the dst directory, otherwise dune might get confused!
        if os.path.isfile(os.path.join(dst, "dune")):
          os.remove(os.path.join(dst, "dune"))

        kernels.append(Kernel(name=kernel_name, type_=kernel_type, has_kernel_cfg=has_kernel_cfg))
    return kernels

def build_target(args):
    target = args.build_target
    assert (target)
    os.chdir(args.build_dir)
    print(f"Running build for target {target} in the build directory {args.build_dir}")
    result = subprocess.run(["make", "build", f"TARGET={target}"])
    if result.returncode != 0:
        raise RuntimeError("Error when building xclbin!")

    if target == "hw_emu" or target == "sw_emu":
        result = subprocess.run(["make", "emconfig", f"TARGET={target}"])
        if result.returncode != 0:
            raise RuntimeError("Error when building emconfig")

def pre_checks(args):

    # If we are running on the AWS platform make sure the user has the platform setup correctly, we also need to export the platform variable
    if (args.platform == "aws"):
        if "AWS_PLATFORM" not in os.environ:
            raise Exception("env var AWS_PLATFORM was not set! Did you correctly source vitis_setup.sh?")
        else:
            aws_platform = os.environ['AWS_PLATFORM']
            delimiter = 'aws_platform'
            aws_platform_root = aws_platform.split(delimiter)[0] + delimiter
            print(f"Setting PLATFORM_REPO_PATHS = {aws_platform_root}")
            os.environ["PLATFORM_REPO_PATHS"] = aws_platform_root


def main():
    args = parser.parse_args()

    pre_checks (args)

    os.makedirs(args.build_dir, exist_ok=True)

    copy_template_files(args)
    kernels = copy_kernel_sources(args)
    copy_supporting_files(args)
    
    write_makefile(args, kernels=kernels)
    write_kernels_mk(build_dir=args.build_dir, kernels=kernels)

    if not args.build_target:
        return

    build_target(args)


if __name__ == "__main__":
    main()
