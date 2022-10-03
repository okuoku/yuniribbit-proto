# FIXME: Detect it
set(ROOT /home/oku/repos/yuni)
set(YUNIBUILD /home/oku/yuni)
set(input /home/oku/repos/yuni/tests/scheme/core0.sps)

set(libargs)
foreach(e lib lib-compat)
    list(APPEND libargs -libpath ${ROOT}/${e})
endforeach()

execute_process(
    COMMAND ${YUNIBUILD}/run-gauche.sh
    -LIBPATH ${ROOT}/lib
    -LIBPATH ${CMAKE_CURRENT_LIST_DIR}
    _expand.sps
    ${libargs}
    -source
    ${input}
    )

