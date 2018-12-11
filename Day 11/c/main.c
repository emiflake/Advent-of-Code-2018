#include <stdio.h>
#include <malloc.h>

#define GRID_SIZE 299

typedef int FuelCells[GRID_SIZE][GRID_SIZE];

/**
 * Get the power level based on the following spec:
 *
 * Find the fuel cell's rack ID, which is its X coordinate plus 10.
 * Begin with a power level of the rack ID times the Y coordinate.
 * Increase the power level by the value of the grid serial number (your puzzle input).
 * Set the power level to itself multiplied by the rack ID.
 * Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
 * Subtract 5 from the power level.
 *
 * @param serial The serial number (puzzle input)
 * @param x The x and y position of the fuel cell
 * @param y
 * @return the powerLevel of the target fuel cell
 */
int powerLevel(int serial, int x, int y) {
    const int rackID = x + 10;

    int power = rackID * (rackID * y + serial);

    return power / 100 % 10 - 5;
}

/**
 * Generate a grid, which will be filled using the powerLevel
 * @param serial The serial number (puzzle input)
 * @return a grid of fuel cells
 */
FuelCells* generate(int serial) {
    FuelCells* f = malloc(sizeof(int) * GRID_SIZE * GRID_SIZE);

    for (int x = 0; x < GRID_SIZE; ++x) {
        for (int y = 0; y < GRID_SIZE; ++y) {
            (*f)[x][y] = powerLevel(serial, x, y);
        }
    }

    return f;
}

/**
 * A helper function to guarantee that our lookup is within bounds, rather than
 * @param grid The grid to get from
 * @param x the x position to access
 * @param y the y position to access
 * @return
 */
int get(FuelCells* grid, int x, int y) {
    if (x > GRID_SIZE) perror("x too big");
    if (y > GRID_SIZE) perror("y too big");
    if (x < 0) perror("x too small");
    if (x < 0) perror("y too small");

    return (*grid)[x][y];
}

typedef struct Maximum {
    /*
     * size, the size of the kernel we looked withs
     */
    int size;

    /*
     * the what the largest power level we found was
     */
    int maximum;

    /*
     * the x and y position of the top-left fuel cell
     */
    int x, y;
} Maximum;

/**
 * Generate a 'Maximum' for our grid, looking in kernels of size x size
 * @param grid the grid to look in
 * @param size the kernel size, size x size
 * @return
 */
Maximum maximumFor(FuelCells* grid, int size) {
    Maximum m = {size, 0, 0, 0};

    for (int x = 0; x < GRID_SIZE - size; ++x) {
        for (int y = 0; y < GRID_SIZE - size; ++y) {
            int count = 0;

            for (int i = 0; i < size; ++i) {
                for (int j = 0; j < size; ++j) {
                    count += get(grid, x + i, y + j);
                }
            }

            if (count > m.maximum) {
                m.maximum = count;
                m.x = x;
                m.y = y;
            }
        }
    }

    return m;
}

int main() {
    FuelCells* grid = generate(4172);

    printf(" ---- PART ONE ---- \n");
    Maximum m1 = maximumFor(grid, 3);
    printf("%i,%i\n", m1.x, m1.y);
    printf(" ---- PART TWO ---- \n");
    Maximum m2 = {1, 0, 0, 0};

    // same thing, 30 is our maximum,
    // but it's an assumption and isn't very conservative
    // if we raise this limit, we can get more assurance that our result is correct
    // but the performance drop will be significant
    for (int sz = 1; sz < 30; ++sz) {
        Maximum tempMax = maximumFor(grid, sz);

        if (tempMax.maximum > m2.maximum) {
            m2 = tempMax;
        }
    }
    printf("%i,%i,%i\n", m2.x, m2.y, m2.size);

    return 0;
}