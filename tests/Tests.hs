module Tests where

import Test.HUnit

import Tarefa1_2021li1g074_Spec
import Tarefa2_2021li1g074_Spec
import Tarefa3_2021li1g074_Spec
import Tarefa4_2021li1g074_Spec
--import Tarefa5_2021li1g074_Spec
import Tarefa6_2021li1g074_Spec
import FuncoesAuxiliares_Tests

runTestsT1 = runTestTT testsT1
runTestsT2 = runTestTT testsT2
runTestsT3 = runTestTT testsT3
runTestsT4 = runTestTT testsT4
--runTestsT5 = runTestTT testsT5
runTestsT6 = runTestTT testsT6
runTestsAux = runTestTT testsAux

runAllTests = runTestTT $ TestList [testsT1, testsT2, testsT3, testsT4 , {-testsT5 ,-} testsT6 , testsAux]