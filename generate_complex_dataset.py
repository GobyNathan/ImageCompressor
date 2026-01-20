#!/usr/bin/env python3

import sys
import random
import math

random.seed(42)

pixels = []

def add_noise(r, g, b, noise_level=5):
    r = max(0, min(255, r + random.randint(-noise_level, noise_level)))
    g = max(0, min(255, g + random.randint(-noise_level, noise_level)))
    b = max(0, min(255, b + random.randint(-noise_level, noise_level)))
    return (r, g, b)

def generate_gradient(r1, g1, b1, r2, g2, b2, steps):
    colors = []
    for i in range(steps):
        t = i / (steps - 1)
        r = int(r1 * (1 - t) + r2 * t)
        g = int(g1 * (1 - t) + g2 * t)
        b = int(b1 * (1 - t) + b2 * t)
        colors.append((r, g, b))
    return colors

# 1. Generate small clusters with slight overlaps
for cluster_id in range(20):
    base_x = random.randint(0, 100)
    base_y = random.randint(0, 100)
    
    if cluster_id % 5 == 0:
        base_r = random.randint(120, 130)
        base_g = random.randint(120, 130)
        base_b = random.randint(120, 130)
    else:
        base_r = random.randint(0, 255)
        base_g = random.randint(0, 255)
        base_b = random.randint(0, 255)
    
    for i in range(50):
        x = base_x + random.randint(-5, 5)
        y = base_y + random.randint(-5, 5)
        
        r, g, b = add_noise(base_r, base_g, base_b)
        
        pixels.append(((x, y), (r, g, b)))

# 2. Add some outliers that can cause centroids to shift minimally
for _ in range(50):
    x = random.randint(0, 150)
    y = random.randint(0, 150)
    r = random.randint(110, 140)
    g = random.randint(110, 140)
    b = random.randint(110, 140)
    pixels.append(((x, y), (r, g, b)))

# 3. Add gradient patterns that have close but distinct colors
gradient_colors = generate_gradient(50, 50, 50, 70, 70, 70, 20)
for i, color in enumerate(gradient_colors):
    x = 150 + i
    for y in range(10):
        r, g, b = add_noise(*color, noise_level=1)
        pixels.append(((x, y), (r, g, b)))

# 4. Add some nearly identical colors in different positions
for i in range(30):
    x = 200 + random.randint(0, 20)
    y = random.randint(0, 20)
    r = 200 + random.randint(0, 1)
    g = 100 + random.randint(0, 1)
    b = 50 + random.randint(0, 1)
    pixels.append(((x, y), (r, g, b)))

random.shuffle(pixels)

for (x, y), (r, g, b) in pixels:
    print(f"({x},{y}) ({r},{g},{b})")

print(f"# Generated {len(pixels)} pixels with complex patterns", file=sys.stderr)