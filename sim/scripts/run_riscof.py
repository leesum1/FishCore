import glob
import os
import subprocess
import utilts
from concurrent.futures import ThreadPoolExecutor

riscof_path = os.path.join("/home/leesum/workhome/riscof-test")
Vtop_path = os.path.join("../build/linux/x86_64/release/Vtop")

build_ret = utilts.execute_command("xmake")

if build_ret["return_code"] != 0:
    print("Build Failed!")
    print(f"Standard Output:\n{build_ret['stdout_output']}")
    print(f"Standard Error:\n{build_ret['stderr_output']}")
    exit(-1)

print("Build Success!")

os.system("cp {} {}".format(Vtop_path, riscof_path))
riscof_ret = utilts.execute_command("cd {} && ./run-test.sh".format(riscof_path))

if riscof_ret["return_code"] != 0:
    print("Riscof Failed!")
    print(f"Standard Output:\n{riscof_ret['stdout_output']}")
    print(f"Standard Error:\n{riscof_ret['stderr_output']}")
    exit(-1)
