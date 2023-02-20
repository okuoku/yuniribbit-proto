cmake_minimum_required(VERSION 3.10)
project(yuniribbit-proto NONE)

set(ROOT /home/oku/repos/yuni)
set(YUNIBUILD /home/oku/yuni)

enable_testing()

set(tests ${ROOT}/tests)

set(testsrcs
    ${tests}/scheme/core0.sps
    ${tests}/scheme/core1.sps
    ${tests}/scheme/core2.sps
    ${tests}/scheme/exp0.sps
    ${tests}/scheme/exp1.sps
    ${tests}/scheme/exp2.sps
    ${tests}/scheme/vectors0.sps
    ${tests}/scheme/bytevectors0.sps
    ${tests}/scheme/inexact0.sps
    ${tests}/scheme/inexact1.sps
    ${tests}/scheme/inexact2.sps
    ${tests}/scheme/iter0.sps
    ${tests}/scheme/qq1.sps
    ${tests}/scheme/stx0.sps
    ${tests}/scheme/strings0.sps
    ${tests}/scheme/values0.sps
    ${tests}/scheme/values1.sps
    ${tests}/scheme/values2.sps
    ${tests}/scheme/values3.sps
    ${tests}/lib/hashtables0.sps
    ${tests}/lib/hashtables1.sps
    ${tests}/lib/minitest0.sps
    ${tests}/lib/yuniio0.sps
    ${tests}/lib/yuniio1.sps
    ${tests}/sibr/err-sibr0012string.sps
    ${tests}/sibr/err-sibr0014.sps
    )

set(negativetestsrcs
    # Run phase
    ${tests}/scheme/inexact3.sps # SIBR0013
    ${tests}/scheme/qq0.sps # ???
    ${tests}/lib/lighteval0.sps # ???
    ${tests}/lib/miniread0.sps # multi-datum read

    # Expand phase
    ${tests}/app/basic/app.sps # lib ?

    # Not ported yet
    ${tests}/sibr/sibr0010string.sps
    ${tests}/sibr/sibr0010vector.sps
    ${tests}/sibr/sibr0011.sps
    ${tests}/sibr/sibr0012gen.sps
    ${tests}/sibr/sibr0013.sps
    ${tests}/sibr/sibr0014.sps

    # fake syntax-rules
    ${tests}/scheme/synrule0.sps
    ${tests}/scheme/synrule1.sps

    # Failure tests
    ${tests}/err/fail0.sps
    ${tests}/err/fail1.sps
    ${tests}/err/fail2.sps
    ${tests}/err/fail3.sps
    ${tests}/err/fail4.sps
    ${tests}/err/fail5.sps
    ${tests}/err/fail6.sps
    ${tests}/err/fail7.sps
    ${tests}/err/fail8.sps
    )
foreach(f ${testsrcs})
    get_filename_component(nam ${f} NAME_WE)

    add_test(NAME ${nam}
        COMMAND ${CMAKE_COMMAND} 
        -Dinput=${f}
        -P ${CMAKE_CURRENT_LIST_DIR}/_testrun.cmake)
endforeach()

foreach(f ${negativetestsrcs})
    get_filename_component(nam ${f} NAME_WE)

    add_test(NAME ${nam}
        COMMAND ${CMAKE_COMMAND} 
        -Dinput=${f}
        -P ${CMAKE_CURRENT_LIST_DIR}/_testrun.cmake)
    set_tests_properties(${nam}
        PROPERTIES WILL_FAIL TRUE)
endforeach()
