#include <iostream>
#include <stack>
#include <vector>
#include <algorithm> // For std::swap

// Partition function
int partition(std::vector<int>& arr, int low, int high) {
    int pivot = arr[high]; // Choose the last element as the pivot
    int i = low - 1; // Index of the smaller element

    for (int j = low; j < high; j++) {
        // If the current element is smaller than or equal to the pivot
        if (arr[j] <= pivot) {
            i++; // Increment the index of the smaller element
            std::swap(arr[i], arr[j]); // Swap elements
        }
    }
    // Swap the pivot element with the element at i+1
    std::swap(arr[i + 1], arr[high]);
    return i + 1; // Return the partition index
}

// Quicksort without recursion
void quickSort(std::vector<int>& arr, int low, int high) {
    // Create a stack to store subarray indices
    std::stack<std::pair<int, int>> stack;
    stack.push(std::make_pair(low, high)); // Push the initial range onto the stack

    // Loop until the stack is empty
    while (!stack.empty()) {
        // Pop the top subarray range
        low = stack.top().first;
        high = stack.top().second;
        stack.pop();

        // Partition the array and get the pivot index
        int p = partition(arr, low, high);

        // Push the left subarray range onto the stack if it has more than one element
        if (p - 1 > low) {
            stack.push(std::make_pair(low, p - 1));
        }

        // Push the right subarray range onto the stack if it has more than one element
        if (p + 1 < high) {
            stack.push(std::make_pair(p + 1, high));
        }
    }
}

int main() {
    // Example array
    std::vector<int> arr = {10, 7, 8, 9, 1, 5};
    int n = arr.size();

    std::cout << "Original array: ";
    for (int i : arr) {
        std::cout << i << " ";
    }
    std::cout << std::endl;

    // Perform Quicksort
    quickSort(arr, 0, n - 1);

    std::cout << "Sorted array: ";
    for (int i : arr) {
        std::cout << i << " ";
    }
    std::cout << std::endl;

    return 0;
}