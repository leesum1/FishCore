import glob
import os
import subprocess
import utilts
from concurrent.futures import ThreadPoolExecutor

tests_path = os.path.join("/opt/riscv-tests/share/riscv-tests/isa")
Vtop_path = os.path.join("../build/linux/x86_64/release/Vtop")

isa_patern = list("rv64u{}-p".format(i) for i in ['i', 'm', 'c'])
isa_patern.append("rv64mi-p")

# 从 tests_path 找到所有以 isa_patern 中元素为开头的文件
isa_patern = [glob.glob(os.path.join(tests_path, "{}*".format(i))) for i in isa_patern]

# 将嵌套列表扁平化
isa_patern = [item for sublist in isa_patern for item in sublist]

# 过滤出文件结尾不为 .dump 的文件
all_tests = list(filter(lambda x: not x.endswith(".dump"), isa_patern))

# 路径拼接
all_tests = [os.path.join(tests_path, i) for i in all_tests]

print("Total Test Count: {}".format(len(all_tests)))



utilts.execute_command("xmake f -m release")
build_ret = utilts.execute_command("xmake")

if build_ret["return_code"] != 0:
    print("Build Failed!")
    print(f"Standard Output:\n{build_ret['stdout_output']}")
    print(f"Standard Error:\n{build_ret['stderr_output']}")
    exit(-1)

print("Build Success!")



def execute_riscv_test(bin_name):
    # 构建命令字符串
    command = f"xmake r Vtop -d --clk=10000000 -f  {bin_name}"

    # 执行命令
    am_test_ret = utilts.execute_command(command, enable_print=False)

    return {
        "bin_file": bin_name,
        "return_code": am_test_ret["return_code"],
        "stdout_output": am_test_ret["stdout_output"],
        "stderr_output": am_test_ret["stderr_output"]
    }




# 使用 ThreadPoolExecutor 并行执行命令
results = []
with ThreadPoolExecutor() as executor:
    futures = [executor.submit(execute_riscv_test, bin_file) for bin_file in all_tests]
    for future in futures:
        result = future.result()
        results.append(result)

results.sort(key=lambda x: x["return_code"])

# 打印所有结果
for result in results:
    bin_name = os.path.basename(result["bin_file"])
    ret_code = result["return_code"]
    print(f"{bin_name}, ret_code: {ret_code}")
