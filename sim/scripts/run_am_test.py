import glob
import os
import utilts

from concurrent.futures import ThreadPoolExecutor

am_test_path = os.path.join("/home/leesum/workhome/ysyx/am-kernels/tests/cpu-tests/build")

# find all .bin files in am_test_path
# Use the glob module to search for all .bin files in the directory
bin_files = glob.glob(os.path.join(am_test_path, "*.bin"))


def execute_am_test(bin_name):
    # 构建命令字符串
    command = f"xmake r Vtop --am --clk=10000000 -f  {bin_name}"

    # 执行命令
    am_test_ret = utilts.execute_command(command, enable_print=False)

    return {
        "bin_file": bin_name,
        "return_code": am_test_ret["return_code"],
        "stdout_output": am_test_ret["stdout_output"],
        "stderr_output": am_test_ret["stderr_output"]
    }


def print_result(result):
    print(f"Command for {result['bin_file']}")
    print(f"Return Code: {result['return_code']}")
    print(f"Standard Output:\n{result['stdout_output']}")
    print(f"Standard Error:\n{result['stderr_output']}")


# crc32_result = execute_am_test(os.path.join(am_test_path, "crc32-riscv64-nemu.elf"))
#
# print_result(crc32_result)

# 使用 ThreadPoolExecutor 并行执行命令
results = []
with ThreadPoolExecutor() as executor:
    futures = [executor.submit(execute_am_test, bin_file) for bin_file in bin_files]
    for future in futures:
        result = future.result()
        results.append(result)

results.sort(key=lambda x: x["return_code"])

# 打印所有结果
for result in results:
    bin_name = os.path.basename(result["bin_file"])
    ret_code = result["return_code"]
    print(f"bin_name: {bin_name}, ret_code: {ret_code}")
