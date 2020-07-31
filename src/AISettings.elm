module AISettings exposing (..)
import Message exposing (..)

type AIMsg =
    AIWalk MoveDirection Bool
    | AICharge Jump Bool

type alias SpeedAIAnim =
    { time: Float
    , msg: AIMsg}

initSpeedAIAnimList =
    [ { msg = AIWalk Right True, time = 1343 }
    , { msg = AIWalk Right False, time = 1598 }
    , { msg = AIWalk Left True, time = 1700 }
    , { msg = AIWalk Left False, time = 1768 }
    , { msg = AICharge Up True, time = 1921 }
    , { msg = AICharge L False, time = 2924 }
    , { msg = AIWalk Left True, time = 3553 }
    , { msg = AIWalk Left False, time = 4369 }
    , { msg = AICharge Up True, time = 4505 }
    , { msg = AICharge L False, time = 6715 }
    , { msg = AIWalk Right True, time = 7803 }
    , { msg = AIWalk Right False, time = 7888 }
    , { msg = AICharge Up True, time = 8024 }
    , { msg = AICharge R False, time = 9826 }
    , { msg = AIWalk Left True, time = 10965 }
    , { msg = AIWalk Left False, time = 11050 }
    , { msg = AICharge Up True, time = 11220 }
    , { msg = AICharge L False, time = 12699 }
    , { msg = AIWalk Right True, time = 13889 }
    , { msg = AIWalk Right False, time = 14654 }
    , { msg = AIWalk Left True, time = 14790 }
    , { msg = AIWalk Left False, time = 14875 }
    , { msg = AICharge Up True, time = 14960 }
    , { msg = AICharge L False, time = 17102 }
    , { msg = AIWalk Right True, time = 18411 }
    , { msg = AIWalk Right False, time = 18496 }
    , { msg = AICharge Up True, time = 18632 }
    , { msg = AICharge R False, time = 20808 }
    , { msg = AIWalk Right True, time = 21539 }
    , { msg = AIWalk Right False, time = 22746 }
    , { msg = AIWalk Left True, time = 22848 }
    , { msg = AIWalk Left False, time = 22950 }
    , { msg = AICharge Up True, time = 22984 }
    , { msg = AICharge L False, time = 25211 }
    , { msg = AIWalk Left True, time = 26605 }
    , { msg = AIWalk Left False, time = 26690 }
    , { msg = AICharge Up True, time = 26775 }
    , { msg = AICharge L False, time = 28339 }
    , { msg = AIWalk Right True, time = 29529 }
    , { msg = AIWalk Right False, time = 30345 }
    , { msg = AICharge Up True, time = 30413 }
    , { msg = AICharge R False, time = 32215 }
    , { msg = AIWalk Right True, time = 33286 }
    , { msg = AIWalk Right False, time = 35394 }
    , { msg = AIWalk Left True, time = 35513 }
    , { msg = AIWalk Left False, time = 35615 }
    , { msg = AICharge Up True, time = 35785 }
    , { msg = AICharge L False, time = 37145 }
    , { msg = AIWalk Left True, time = 38131 }
    , { msg = AIWalk Left False, time = 38233 }
    , { msg = AICharge Up True, time = 38403 }
    , { msg = AICharge L False, time = 40103 }
    , { msg = AIWalk Right True, time = 41463 }
    , { msg = AIWalk Right False, time = 41650 }
    , { msg = AIWalk Left True, time = 41701 }
    , { msg = AIWalk Left False, time = 41803 }
    , { msg = AICharge Up True, time = 42007 }
    , { msg = AICharge L False, time = 43843 }
    , { msg = AIWalk Left True, time = 45084 }
    , { msg = AIWalk Left False, time = 45186 }
    , { msg = AICharge Up True, time = 45543 }
    , { msg = AICharge L False, time = 47940 }
    , { msg = AIWalk Right True, time = 49096 }
    , { msg = AIWalk Right False, time = 50014 }
    , { msg = AIWalk Left True, time = 50048 }
    , { msg = AIWalk Left False, time = 50133 }
    , { msg = AICharge Up True, time = 50320 }
    , { msg = AICharge L False, time = 51816 }
    , { msg = AIWalk Left True, time = 52632 }
    , { msg = AIWalk Left False, time = 55250 }
    , { msg = AIWalk Right True, time = 55454 }
    , { msg = AIWalk Right False, time = 55539 }
    , { msg = AICharge Up True, time = 55726 }
    , { msg = AICharge R False, time = 57851 }
    , { msg = AIWalk Right True, time = 58582 }
    , { msg = AIWalk Right False, time = 60231 }
    , { msg = AIWalk Left True, time = 60299 }
    , { msg = AIWalk Left False, time = 60418 }
    , { msg = AICharge Up True, time = 60605 }
    , { msg = AICharge L False, time = 62118 }
    ]