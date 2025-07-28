cmake_minimum_required(VERSION 3.15)
project(Kaleidoscope)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Add all source files
add_executable(Kaleidoscope 
    main.cpp
    Lexer.cpp
    Parser.cpp
)

# Set C++23 features for std::print
target_compile_features(Kaleidoscope PRIVATE cxx_std_23)
