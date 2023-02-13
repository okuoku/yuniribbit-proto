# FIXME: Detect it
set(ROOT /home/oku/repos/yuni)
set(YUNIBUILD /home/oku/yuni)
#set(input /home/oku/repos/yuni/tests/scheme/core0.sps)
#set(input ${CMAKE_CURRENT_LIST_DIR}/check4.sps)

set(libargs)
foreach(e lib lib-compat lib-r7c)
    list(APPEND libargs -libpath ${ROOT}/${e})
endforeach()

set(impl gauche)

# Generate pass paths
get_filename_component(nam ${input} NAME_WE)
set(expanded ${nam}.expand.bin)
set(compiled ${nam}.compiled.bin)

# Expand
execute_process(
    COMMAND ${YUNIBUILD}/run-${impl}.sh
    -LIBPATH ${ROOT}/lib
    -LIBPATH ${CMAKE_CURRENT_LIST_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/_expand.sps
    -libpath ${CMAKE_CURRENT_LIST_DIR}/runtime
    ${libargs}
    -source
    ${input}
    -out
    ${expanded}
    RESULT_VARIABLE rr
    )

if(rr)
    message(FATAL_ERROR "Expand error: ${rr}")
endif()

# Compile
execute_process(
    COMMAND ${YUNIBUILD}/run-${impl}.sh
    -LIBPATH ${ROOT}/lib
    -LIBPATH ${CMAKE_CURRENT_LIST_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/_compile.sps
    -in
    ${expanded}
    -out
    ${compiled}
    RESULT_VARIABLE rr
    )

if(rr)
    message(FATAL_ERROR "Compile error: ${rr}")
endif()

# Run
execute_process(
    COMMAND ${YUNIBUILD}/run-${impl}.sh
    -LIBPATH ${ROOT}/lib
    -LIBPATH ${CMAKE_CURRENT_LIST_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/_vm.sps
    -run
    ${compiled}
    RESULT_VARIABLE rr
    )

if(rr)
    message(FATAL_ERROR "Run error: ${rr}")
endif()

