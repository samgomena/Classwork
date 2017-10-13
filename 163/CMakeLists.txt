#cmake_minimum_required(VERSION 3.7)
#project(WeatherApp)
#
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
#
#add_custom_target(WeatherApp COMMAND make -C ${mytest_SOURCE_DIR} CLION_EXE_DIR=${PROJECT_BINARY_DIR})

cmake_minimum_required(VERSION 3.6)
project(project4)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wall")

set(SOURCE_FILES
        Classwork/project4/plant.h
        Classwork/project4/plant.cpp
        Classwork/project4/planttree.h
        Classwork/project4/planttree.cpp
        Classwork/project4/treenode.h
        Classwork/project4/treenode.cpp
        Classwork/project4/experiments.cpp)


add_executable(project4 ${SOURCE_FILES})