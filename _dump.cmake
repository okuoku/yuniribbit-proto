# FIXME: Detect it
set(ROOT /home/oku/repos/yuni)
set(YUNIBUILD /home/oku/yuni)

math(EXPR arg "${CMAKE_ARGC}-1")
set(input "${CMAKE_ARGV${arg}}")

set(impl gauche)

execute_process(
    COMMAND ${YUNIBUILD}/run-${impl}.sh
    -LIBPATH ${ROOT}/lib
    -LIBPATH ${CMAKE_CURRENT_LIST_DIR}
    _dump.gauche.sps
    ${input}
    RESULT_VARIABLE rr
    )

