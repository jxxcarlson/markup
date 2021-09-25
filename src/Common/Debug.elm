module Common.Debug exposing (debug1, debug2, debug3)

import Console


debugOn =
    False


debug1 str =
    if debugOn then
        Debug.log (Console.magenta str)

    else
        identity


debug2 str =
    if debugOn then
        Debug.log (Console.cyan str)

    else
        identity


debug3 str =
    if debugOn then
        Debug.log (Console.yellow str)

    else
        identity
