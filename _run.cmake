# FIXME: Detect it
set(ROOT /home/oku/repos/yuni)
set(YUNIBUILD /home/oku/yuni)

math(EXPR arg "${CMAKE_ARGC}-1")
set(cur 0)
set(argstate pre)
set(interparg)
while(NOT cur EQUAL ${CMAKE_ARGC})
    #message(STATUS "ARG(${argstate}): ${CMAKE_ARGV${cur}}")
    if(argstate STREQUAL pre)
        if(${CMAKE_ARGV${cur}} STREQUAL -P)
            math(EXPR cur "${cur}+1")
            set(argstate target)
        endif()
    elseif(argstate STREQUAL target)
        set(file ${CMAKE_ARGV${cur}})
        set(argstate interp)
        if(${file} STREQUAL BOOT)
            set(bootfile ${CMAKE_CURRENT_LIST_DIR}/boot/start.sps)
            set(bootstrap -bootstrap ${bootfile})
            set(source)
        elseif(${file} STREQUAL COMPILE)
            math(EXPR cur "${cur}+1")
            set(source -out dump.bin -source ${CMAKE_ARGV${cur}})
            set(bootstrap)
        else()
            set(source -source ${file})
            set(bootstrap)
        endif()
    else()
        list(APPEND interparg ${CMAKE_ARGV${cur}})
    endif()
    math(EXPR cur "${cur}+1")
endwhile()

if(interparg)
    set(restarg -- ${interparg})
else()
    set(restarg)
endif()

if(NOT source AND NOT bootstrap)
    set(input "${ROOT}/tests/scheme/core0.sps")
    message(STATUS "running ${input}")
    set(bootstrap)
    set(source -source ${input})
endif()

set(libargs)
foreach(e lib lib-compat lib-r7c)
    list(APPEND libargs -libpath ${ROOT}/${e})
endforeach()

set(impl gauche)

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
    # Actual arguments
    ${source}
    ${bootstrap}
    ${restarg}
    RESULT_VARIABLE rr
    )

if(rr)
    message(FATAL_ERROR "Run error: ${rr}")
endif()

