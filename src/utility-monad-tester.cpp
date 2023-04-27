#include "utility-monad-tester.hpp"

int main()
{
	std::cout << "Hello CMake." << std::endl;

	std::cout << "Hello Monad." << std::endl;
	util::tests::test_monad();

	return 0;
}
