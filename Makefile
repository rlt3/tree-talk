engine:
	clang -c script.c -o obj/script.o -I/usr/local/include/ -F/Library/Frameworks
	clang++ -std=c++11 -c instance.cpp -o obj/instance.o -I/usr/local/include/ -F/Library/Frameworks
	clang++ -std=c++11 -c component.cpp -o obj/component.o -I/usr/local/include/ -F/Library/Frameworks
	clang++ -std=c++11 -c main.cpp -o obj/main.o -I/usr/local/include/ -F/Library/Frameworks
	clang++ -std=c++11 -o engine obj/main.o obj/component.o obj/instance.o obj/script.o -I/usr/include/ -L/usr/local/lib/ -llua -I/usr/include/ 

clean:
	rm -f engine obj/*.o
