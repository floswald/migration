
#include <iostream>
#include "cowner.h"
#include <vector>

int main(int argument_count, char ** command_line_arguments)
{ 
	std::cout << "The main program is running" << std::endl;

	double data[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	TinyVector<int,4> dims = shape(2,2,2,2);
	TinyVector<int,3> dims2 = shape(2,2,2);

	// create an instance of owner class
	COwner4 t4(dims,dims2,data);
	
	std::cout << "An instance of an owner class was created:" << std::endl;
	
	t4.show();
	std::cout << std::endl;
	std::cout << "class type:" << std::endl;
	t4.version();


	// create instance of 5 dimesional thing
	double data2[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
	                 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32};
	double data3[] = {17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
	                 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};

	TinyVector<int,5> dims3 = shape(2,2,2,2,2);

	// create an instance of owner class
	COwner5 t5(dims3,data2,data3);
	
	t5.show();
	
	std::cout << std::endl;
	std::cout << "class type:" << std::endl;
	t5.version();
	
	
	return 0;
}
