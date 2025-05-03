import random
n = 100_000 # Generate 100,000 values
print(f"Int{{{",".join([f"{random.random() * 1e6:.0f}" for i in range(n)])}}}")
