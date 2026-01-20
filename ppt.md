# Image Compressor
## Making Photos Smaller Without Losing Quality

---

## What Is This Project?

- A tool that makes digital images smaller
- Works by reducing the number of colors in an image
- Similar to how many photo apps compress images
- Saves storage space while keeping images looking good

---

## Why Do We Need Image Compression?

- Digital photos can be very large files
- Smaller files:
  - Load faster on websites
  - Take up less storage space
  - Can be shared more easily
- Our solution: Smart color reduction

---

## How It Works: The Big Picture

Three simple steps:
1. Look at all the colors in an image
2. Group similar colors together
3. Replace each group with one representative color

Think of it like sorting a bag of similar-colored candies into groups!

---

## The Magic: K-means Clustering

- A way to organize things into groups
- "K" = the number of groups we want
- Steps:
  - Pick some starting points
  - Group similar colors with the closest point
  - Find the average color in each group
  - Repeat until the groups stabilize

---

## Example: Compressing a Sunset Photo

Original sunset photo might have:
- Thousands of slightly different reds and oranges
- Many shades of blue in the sky
- Various yellow tints in the sun

After compression:
- Just 16 carefully chosen colors
- Still looks like a beautiful sunset to our eyes!

---

## How We Decide "Similar" Colors

- Color distance = how different two colors look
- Think of colors as points in 3D space:
  - Red, Green, Blue coordinates
  - Similar colors are close together
  - Different colors are far apart
- We measure the straight-line distance between colors

---

## Project Structure: The Building Blocks

- **Types**: Definitions of colors and points
- **K-means**: The grouping algorithm
- **Parser**: Reads image data
- **Output**: Creates the final result
- **Command-line Interface**: How users interact with the program

---

## Making It Fast: Parallel Processing

- Uses multiple computer processors at once
- Like having several people sort colors instead of just one
- Much faster for large images
- Modern computers have multiple "brains" (cores)
- Our project takes advantage of all available cores

---

## How to Use It

Simple command:
```
./imageCompressor -n 16 -l 0.8 -f my_photo.txt
```

Where:
- 16 = number of colors we want in the final image
- 0.8 = how precise the color grouping should be
- my_photo.txt = the image file to compress

---

## What the Program Reads

The program reads files containing pixel information:

- Each line describes one dot in the image
- Format: position (x,y) and color (r,g,b)
- Example: (0,0) (33,18,109) means:
  - Top-left pixel
  - Deep blue-purple color

---

## What the Program Creates

After processing, it outputs:
- Groups of similar colors
- Each group has:
  - One "representative" color (average of all colors in group)
  - List of all original pixels in that group
- This information can be used to create the compressed image

---

## Technical Tools Used

- **Haskell**: A specialized programming language
- **Stack**: A tool for organizing the project
- **Parallel Processing**: For better speed
- All packaged in a simple, easy-to-use program

---

## Future Improvements

- Direct reading of image files (JPG, PNG)
- Live preview of compression
- More intelligent color selection
- Different ways to measure color similarity

---

## What We've Learned

- Image compression is a balance between file size and quality
- Grouping similar colors is an effective strategy
- Computers can find patterns that make sense to human eyes
- Smart algorithms can create small files that still look good

---

## Thank You!

Any Questions?