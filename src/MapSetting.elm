module MapSetting exposing(..)

type alias Pos =
    { x1: Float
    , x2: Float
    , y1: Float
    , y2: Float }
    
--One
brickPosList1 =
    [ Pos 3200 10000 3900 4000
    , Pos 2900 3200 3800 3935 --[1600,3200]*[3200,4000]
    , Pos 2350 2650 3635 3700
    , Pos 1800 2100 3470 3535
    , Pos 2800 3100 3300 3365
    , Pos 2250 2500 3175 3240 --[1600,3200]*[3200,4000]
    , Pos 1700 2000 3035 3100 --[1600,3200]*[2400,3200]
    , Pos 2700 3000 2900 3000
    , Pos 2175 2475 2700 2800
    , Pos 1700 2000 2350 2485 --[1600,3200]*[2400,3200]
    , Pos 2250 2450 2150 2250 --[1600,3200]*[1600,2400]
    , Pos 2600 3050 1950 2050 --
    , Pos 2000 2500 1450 1700
    , Pos 2500 2750 1750 1800 --[1600,3200]*[1600,2400]//
    , Pos 1900 2300 1160 1215 --[1600,3200]*[800,1600]
    , Pos 2500 2900 1160 1215
    , Pos 1900 2300 790 930
    , Pos 2500 2900 790 930  --[1600,3200]*[800,1600]
    ]
brickWallList1 = 
    [ Pos 0 1 3200 3935 
    , Pos 0 1500 3100 3200
    , Pos 1700 3200 0 100
    , Pos 0 3135 3935 4000
    , Pos 1600 1700 0 3200
    , Pos 3170 3200 775 4000
    , Pos 3200 3900 750 875
    ]
    
characterPosList1 =
    [ Pos 2900 3000 3205 3305
    , Pos 1800 1900 2935 3035
    , Pos 1800 1900 2250 2350
    , Pos 2000 2100 1075 1175
    , Pos 2700 2800 1075 1175
    , Pos 2430 2530 3540 3640  --debug
    ]

npcPosList1 = 
    [ Pos  3100 3200 3695 3795
    ]

birdPosList1 = 
    [ Pos 300 400 3830 3930
    ]

playerPos1 = Pos 300 400 3830 3930

exitPos1 = Pos 3200 10000 2000 3900

--DiscoverI
brickPosListDiscoverI = 
    [  Pos 400 600 3700 3765   --[0,1600]*[3200,4000]
    , Pos 800 1000 3475 3540
    , Pos 1200 1500 3175 3275
    , Pos 100 200 3195 3395   --[0,1600]*[3200,4000]
    , Pos 1325 1375 2975 3175 --[0,1600]*[2400,3200]
    , Pos 650 950 2900 3000
    , Pos 775 825 2700 2900
    , Pos 100 400 2675 2775
    , Pos 225 275 2475 2675   --[0,1600]*[2400,3200]
    , Pos 350 650 2200 2265   --[0,1600]*[1600,2400]
    , Pos 850 1150 2000 2065
    , Pos 1300 1500 1800 1865
    , Pos 300 1100 1550 1650
    , Pos 100 250 1800 2200   --[0,1600]*[1600,2400]
    , Pos 500 750 1250 1400   --[0,1600]*[800,1600]
    , Pos 1000 1250 950 1100
    , Pos 1400 1500 750 900   --[0,1600]*[800,1600]
    , Pos 1150 1300 600 700   --[0,1600]*[0,800]
    , Pos 2500 2550 300 850   --[1600,3200]*[0,800]
    , Pos 2650 2950 400 850   --[1600,3200]*[0,800]
    ]
brickWallListDiscoverI = 
    [ Pos 0 100 0 4000
    , Pos 1500 1600 750 4000
    , Pos 1600 1800 750 850
    , Pos 100 3200 0 100
    , Pos 100 1500 3935 4000
    ]

characterPosListDiscoverI = []

npcPosListDiscoverI = 
    [ 


    ]

birdPosListDiscoverI = 
    [

        
    ]

playerPosDiscoverI = Pos 200 300 3830 3930

exitPosDiscoverI = Pos 1600 10000 2400 3000

--Two
brickPosList2 = 
    [ Pos 550 700 4585 4735   --[0,1600]*[4000,4800]
    , Pos 900 1050 4585 4735
    , Pos 550 700 4200 4450
    , Pos 900 1050 4200 4450  --[0,1600]*[4000,4800]
    , Pos 100 200 3900 4100   --[0,1600]*[3200,4000]
    , Pos 1400 1500 3900 4100
    , Pos 550 750 3700 3800
    , Pos 850 1050 3700 3800
    , Pos 725 875 3450 3515   --[0,1600]*[3200,4000]
    , Pos 250 600 3150 3250   --[0,1600]*[2400,3200]
    , Pos 1000 1350 3150 3250
    , Pos 550 1050 2900 3000
    , Pos 100 300 2700 2765
    , Pos 750 1200 2350 2450  --[0,1600]*[2400,3200]
    , Pos 1300 1500 2100 2200 --[0,1600]*[1600,2400]
    , Pos 1300 1500 1750 1850
    , Pos 500 750 2000 2100
    , Pos 500 750 1650 1750
    , Pos 1000 1050 850 2200  --[0,1600]*[1600,2400]
    , Pos 1300 1500 1400 1500 --[0,1600]*[800,1600]
    , Pos 1300 1500 1000 1100
    , Pos 500 750 1300 1400
    , Pos 100 300 1100 1250
    , Pos 575 1200 785 850    --[0,1600]*[800,1600]
    , Pos 875 1500 500 600    --[0,1600]*[0,800]
    , Pos 700 900 250 325     --[0,1600]*[0,800]
    ]

brickWallList2 = 
    [ Pos 0 100 0 4800
    , Pos 100 1500 4735 4800
    , Pos 1500 1600 0 4800
    ]

characterPosList2 = 
    [ Pos 750 850 150 250
    ]

npcPosList2 = 
    [ 


    ]

birdPosList2 = 
    [

        
    ]

playerPos2 = Pos 300 400 4630 4730  -- the pos of AI is 1200 1300 4630 4730

speedAIPos2 = Pos 1200 1300 4630 4730

exitPos2 = Pos 750 850 150 250

--DiscoverII
brickPosListDiscoverII = 
    [ Pos 800 1300 3600 3700  --[0,1600]*[3200,4000]
    , Pos 800 950 3250 3600
    , Pos 800 1300 3195 3295
    , Pos 200 600 3150 3250   --[0,1600]*[3200,4000]
    , Pos 100 200 2900 3000   --[0,1600]*[2400,3200]
    , Pos 350 550 2700 2950
    , Pos 100 350 2450 2550
    , Pos 550 800 2350 2950
    , Pos 1050 1500 2845 3000
    , Pos 1400 1500 2525 2625
    , Pos 1250 1400 2525 2575 --[0,1600]*[2400,3200]
    , Pos 100 400 2100 2200   --[0,1600]*[1600,2400]
    , Pos 100 550 2000 2100
    , Pos 200 300 1900 2000
    , Pos 100 200 1550 2000
    , Pos 1400 1500 1550 1700 --[0,1600]*[1600,2400]
    , Pos 400 500 1300 1365   --[0,1600]*[800,1600]
    , Pos 100 200 1050 1115
    , Pos 500 600 785 1150
    , Pos 500 700 1150 1650
    , Pos 1100 1500 1150 1550 --[0,1600]*[800,1600]
    , Pos 800 1200 500 850   --[0,1600]*[0,800]
    ]

brickWallListDiscoverII = 
    [ Pos 0 100 0 4000
    , Pos 1500 1600 0 4000
    , Pos 100 1500 0 100
    , Pos 100 1500 3935 4000
    ]

characterPosListDiscoverII = []

npcPosListDiscoverII = 
    [ 


    ]

birdPosListDiscoverII = 
    [

        
    ]

playerPosDiscoverII = Pos 200 300 3830 3930

exitPosDiscoverII = Pos 900 1100 400 500 

--Three
brickPosList3 =
    [ Pos 500 800 4600 4735   --[0,1600]*[4000,4800]
    , Pos 100 350 4250 4350
    , Pos 850 1050 4200 4300
    , Pos 1300 1500 3975 4075 --[0,1600]*[4000,4800]
    , Pos 250 500 3835 3900   --[0,1600]*[3200,4000]
    , Pos 1000 1200 3650 3735
    , Pos 650 850 3535 3600
    , Pos 250 800 3175 3275
    , Pos 1100 1400 3315 3415 --[0,1600]*[3200,4000]
    , Pos 100 350 2900 3000   --[0,1600]*[2400,3200]
    , Pos 1200 1500 2935 3000
    , Pos 900 1000 2965 3000
    , Pos 1350 1500 2700 2765
    , Pos 900 1200 2700 2765
    , Pos 550 775 2600 2670
    , Pos 500 550 2575 2670
    , Pos 250 550 2515 2575   --[0,1600]*[2400,3200]
    , Pos 950 1300 2200 2225  --[0,1600]*[1600,2400]
    , Pos 950 1150 2225 2425
    , Pos 950 1300 2425 2450
    , Pos 1250 1300 2100 2200
    , Pos 1350 1500 1800 1865
    , Pos 600 950 1650 2300
    , Pos 100 600 2200 2300
    , Pos 350 600 1900 1965
    , Pos 100 250 1705 1770
    , Pos 600 1250 1550 1650  --[0,1600]*[1600,2400]
    , Pos 250 750 1200 1300   --[0,1600]*[800,1600]
    , Pos 1250 1500 1250 1350
    , Pos 900 1250 950 1050   --[0,1600]*[800,1600]
    , Pos 100 600 700 800     --[0,1600]*[0,800]
    , Pos 400 1200 300 400    --[0,1600]*[0,800]
    , Pos -1600 0 1900 2000   -- secret room
    , Pos -1600 -1500 2000 4000
    , Pos -1500 -100 3900 4000
    , Pos -100 0 2200 4000
    , Pos -250 -100 2530 3200
    , Pos -400 -250 2610 3200
    , Pos -550 -400 2690 3200
    , Pos -700 -550 2770 3200
    , Pos -850 -700 2850 3200
    , Pos -1000 -850 2930 3200
    , Pos -1150 -1000 3010 3200
    , Pos -1300 -1150 3090 3200
    , Pos -1500 -1200 3500 3900
    , Pos -1200 -900 3700 3900
    , Pos -600 -100 3800 3900
    ]

brickWallList3 = 
    [ Pos 0 100 0 2000
    , Pos 0 100 2200 4800
    , Pos 100 1500 4735 4800
    , Pos 1500 1600 0 4800 
    ]

characterPosList3 =
    [ Pos 175 275 4150 4250
    , Pos 300 400 3735 3835
    , Pos 300 400 3075 3175
    , Pos 1200 1300 3215 3315
    , Pos 1300 1400 2835 2935
    , Pos 1000 1100 2600 2700
    , Pos 620 720 2500 2600
    , Pos 1050 1150 2100 2200
    , Pos 150 250 2100 2200
    , Pos 400 500 1800 1900
    , Pos 125 225 1605 1705 --need to adjust range
    , Pos 650 750 1450 1550
    , Pos 800 900 1450 1550
    , Pos 400 500 1100 1200
    , Pos 425 525 200 300
    , Pos 1075 1175 200 300
    , Pos 750 850 100 300
    , Pos -500 -400 3700 3800 -- secret room
    , Pos -300 -200 3700 3800
    ]

npcPosList3 = 
    [ 


    ]

birdPosList3 = 
    [

        
    ]

playerPos3 = Pos 200 300 4630 4730

speedAIPos3 = Pos 1300 1400 4630 4730

exitPos3 = Pos 2000 2100 0 100