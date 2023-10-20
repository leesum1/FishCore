import subprocess


def execute_command(cmd, enable_print=True):
    process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                               universal_newlines=True)

    stdout_output = ""
    for line in iter(process.stdout.readline, ''):
        if enable_print:
            print(line, end='')  # 打印输出
        stdout_output += line

    return_code = process.wait()  # 等待进程完成

    return {
        "return_code": return_code,
        "stdout_output": stdout_output,
        "stderr_output": ""  # 由于我们将stderr重定向到stdout，所以这里为空
    }
