cmake_minimum_required(VERSION 3.10)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON) 

project(MemoryManager)

add_library(${PROJECT_NAME} SHARED memory.cpp memory.h api.h)
add_executable(${PROJECT_NAME}-benchmark benchmark.cpp)