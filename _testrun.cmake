# FIXME: Detect it
set(ROOT /home/oku/repos/yuni)
set(YUNIBUILD /home/oku/yuni)

set(libargs)
foreach(e lib lib-compat lib-r7c)
    list(APPEND libargs -libpath ${ROOT}/${e})
endforeach()

set(impl gauche)

# Generate pass paths
if(interp)
    set(restargs
        -bootstrap
        ${CMAKE_CURRENT_LIST_DIR}/boot/start.sps
        --
        ${input})
else()
    get_filename_component(nam ${input} NAME_WE)
    set(expanded ${nam}.expand.bin)
    set(compiled ${nam}.compiled.bin)
    set(restargs -source ${input})
endif()

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
    ${restargs}
    RESULT_VARIABLE rr
    )

if(rr)
    message(FATAL_ERROR "Run error: ${rr}")
endif()

