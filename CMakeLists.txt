cmake_minimum_required(VERSION 4.0)
project(Kaleidoscope)
set(CMAKE_CXX_STANDARD 26)

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
add_executable(Kaleidoscope main.cpp Lexer.cpp
        Parser.cpp
        AST.cpp
        Optimize.cpp
        Optimize.cpp
        "control flow.cpp"
)
# 链接LLVM库
llvm_map_components_to_libnames(LLVM_LIBS core support irreader)
target_link_libraries(Kaleidoscope ${LLVM_LIBS} ${ZLIB_LIBRARY})
