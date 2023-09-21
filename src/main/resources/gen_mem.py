import random

# Parameters for memory size and output file name
MEMORY_SIZE = 64
OUTPUT_FILE = "random_data_readmemh.txt"

# Generate random data and write it to the output file
with open(OUTPUT_FILE, "w") as file:
    for _ in range(MEMORY_SIZE):
        random_data = random.randint(0, 18446744073709551615)  # Generate a random 64-bit value (0-2^64-1)
        file.write(f"{random_data:016X}\n")     # Write the data in hexadecimal format

print(f"Random data written to {OUTPUT_FILE}")
