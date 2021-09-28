module Common.Debug exposing (debug1, debug2, debug3)

import Console


debugOn =
    True


debug1 str =
    identity


debug2 str =
    identity


debug3 str =
    identity



--debug1 str =
--    if debugOn then
--        Debug.log (Console.magenta str)
--
--    else
--        identity
--
--
--debug2 str =
--    if debugOn then
--        Debug.log (Console.cyan str)
--
--    else
--        identity
--
--
--debug3 str =
--    if debugOn then
--        Debug.log (Console.yellow str)
--
--    else
--        identity
