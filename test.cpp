#include <iostream>

extern "C" {
	int f(int);
}

int main() {
	for(int i = 0;i<11;i++)
		std::cout << "f(" << i << ")" << f(i) << std::endl;
}