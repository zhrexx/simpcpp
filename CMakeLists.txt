cmake_minimum_required(VERSION 4.0)
project(simpc__)

set(CMAKE_CXX_STANDARD 23)

add_executable(simpc__ example.cpp
        simp.h
        parts/tuple.h)
