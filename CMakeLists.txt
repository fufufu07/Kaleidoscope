cmake_minimum_required(VERSION 3.26)

# 强制使用 Clang 编译器
set(CMAKE_C_COMPILER "C:/msys64/clang64/bin/clang.exe")
set(CMAKE_CXX_COMPILER "C:/msys64/clang64/bin/clang++.exe")

project(Kaleidoscope)

# 使用 C++23
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# 添加LLVM支持
set(LLVM_DIR "C:/msys64/clang64/lib/cmake/llvm")
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include_directories(${LLVM_INCLUDE_DIRS})

# 添加LLVM Kaleidoscope示例路径
include_directories("K:/code/C++/llvm-project-main/llvm/examples/Kaleidoscope/include")

# 手动指定ZLIB路径（修正为libz.dll.a）
set(ZLIB_ROOT "C:/msys64/clang64")
set(ZLIB_INCLUDE_DIR "${ZLIB_ROOT}/include")
set(ZLIB_LIBRARY "${ZLIB_ROOT}/lib/libz.dll.a")
find_package(ZLIB REQUIRED)
include_directories(${ZLIB_INCLUDE_DIR})

add_executable(Kaleidoscope
        main.cpp
        Lexer.cpp
        Parser.cpp
        AST.cpp
        Optimize.cpp
        "control flow.cpp"
        Debug.cpp
        Debug.h)

# 链接LLVM库（添加更多必要组件）
llvm_map_components_to_libnames(LLVM_LIBS
    core
    support
    irreader
    executionengine
    mcjit
    target
    x86codegen
    x86asmparser
    x86info
    passes
    analysis
    transformutils)
target_link_libraries(Kaleidoscope ${LLVM_LIBS} ${ZLIB_LIBRARY})
