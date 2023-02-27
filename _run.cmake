# FIXME: Detect it
set(ROOT /home/oku/repos/yuni)
set(YUNIBUILD /home/oku/yuni)

math(EXPR arg "${CMAKE_ARGC}-1")
set(input "${CMAKE_ARGV${arg}}")
get_filename_component(ext ${input} EXT)
if(ext STREQUAL .cmake)
    set(input "${ROOT}/tests/scheme/core0.sps")
    message(STATUS "running ${input}")
endif()

set(libargs)
foreach(e lib lib-compat lib-r7c)
    list(APPEND libargs -libpath ${ROOT}/${e})
endforeach()

set(impl gauche)

# Generate pass paths
get_filename_component(nam ${input} NAME_WE)
set(expanded ${nam}.expand.bin)
set(compiled ${nam}.compiled.bin)

# Run
execute_process(
    COMMAND ${YUNIBUILD}/run-${impl}.sh
    -LIBPATH ${ROOT}/lib
    -LIBPATH ${CMAKE_CURRENT_LIST_DIR}
    -LIBPATH ${CMAKE_CURRENT_LIST_DIR}/emul
    ${CMAKE_CURRENT_LIST_DIR}/_immvm.sps
    -libpath ${CMAKE_CURRENT_LIST_DIR}/runtime
    -libpath ${CMAKE_CURRENT_LIST_DIR}
    -libpath ${ROOT}/external
    ${libargs}
    -source
    ${input}
    RESULT_VARIABLE rr
    )

if(rr)
    message(FATAL_ERROR "Run error: ${rr}")
endif()

