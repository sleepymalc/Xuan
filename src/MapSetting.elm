module MapSetting exposing(..)

type alias Pos =
    { x1: Float
    , x2: Float
    , y1: Float
    , y2: Float }
    
--One
brickPosList1 =
    [ Pos 3200 10000 3900 4000
    , Pos 2350 2650 3635 3700
    , Pos 1800 2100 3470 3535
    , Pos 2800 3100 3300 3365
    , Pos 2250 2500 3175 3200 --[1600,3200]*[3200,4000]
    , Pos 1700 2000 3035 3100 --[1600,3200]*[2400,3200]
    , Pos 2700 3000 2900 3000
    , Pos 2175 2475 2700 2800
    , Pos 1700 2000 2350 2485 --[1600,3200]*[2400,3200]
    , Pos 2250 2450 2150 2200 --[1600,3200]*[1600,2400]
    , Pos 2600 3050 1950 2050 
    , Pos 2000 2500 1450 1550
    , Pos 2500 2750 1750 1800 --[1600,3200]*[1600,2400]//
    , Pos 1900 2300 1160 1215 --[a1600,3200]*[800,1600]
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
    , Pos 2900 3200 3800 3935
    ]
    
characterPosList1 =
    [ Pos 2900 3000 3205 3305
    , Pos 1800 1900 2935 3035
    , Pos 1800 1900 2250 2350
    , Pos 2000 2100 1060 1160
    , Pos 2700 2800 1060 1160
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
    [  Pos 400 600 3700 3765  --[0,1600]*[3200,4000]
    , Pos 800 1000 3475 3540
    , Pos 0 200 3195 3245     --[0,1600]*[3200,4000]
    , Pos 1200 1325 3175 3200 --[0,1600]*[2400,3200]
    --, Pos 1325 1375 2975 3175 
    , Pos 650 775 2900 2950
    --, Pos 775 825 2700 2905
    , Pos 275 400 2685 2735
    , Pos 200 275 2485 2535   --[0,1600]*[2400,3200]
    , Pos 350 650 2200 2265   --[0,1600]*[1600,2400]
    , Pos 850 1150 2000 2065
    , Pos 1300 1500 1800 1865
    , Pos 300 1100 1550 1600
    , Pos 0 250 2100 2150   --[0,1600]*[1600,2400]
    , Pos 500 750 1250 1300   --[0,1600]*[800,1600]
    , Pos 1000 1250 950 1100
    , Pos 1400 1600 750 800   --[0,1600]*[800,1600]
    , Pos 1600 1900 750 800
    , Pos 1150 1300 600 700   --[0,1600]*[0,800]
    , Pos 2650 2950 400 3200   --[1600,3200]*[0,800]
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
    , Pos 1000 1050 800 2200  --[0,1600]*[1600,2400]
    , Pos 1300 1500 1400 1500 --[0,1600]*[800,1600]
    , Pos 1300 1500 1000 1100
    , Pos 500 750 1300 1400
    , Pos 100 300 1100 1250
    , Pos 575 1200 750 800    --[0,1600]*[800,1600]
    , Pos 875 1500 500 550    --[0,1600]*[0,800]
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
    [ Pos 1100 1300 3600 3650 --[0,1600]*[3200,4000]
    , Pos 900 1300 3195 3245
    , Pos 400 600 3150 3200   --[0,1600]*[3200,4000]
    , Pos 100 200 2900 2950   --[0,1600]*[2400,3200]
    , Pos 350 450 2700 2750
    , Pos 200 300 2530 2580
    , Pos 850 1300 2845 2895
    , Pos 500 800 2900 2950
    , Pos 1250 1500 2790 2840 --[0,1600]*[2400,3200]
    , Pos 750 850  1900 1950  --[0,1600]*[1600,2400]
    , Pos 750 850 2300 2350   
    , Pos 1300 1400 2050 2100 --[0,1600]*[1600,2400]
    , Pos 200 300 1550 1600   --[0,1600]*[800,1600]
    , Pos 500 700 1350 1400
    , Pos 100 200 1050 1115
    , Pos 1400 1500 1150 1200 --[0,1600]*[800,1600]
    , Pos 800 1600 500 550    --[0,1600]*[0,800]
    , Pos 450 550 650 700
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

exitPosDiscoverII = Pos 1400 1600 400 500 

--Three
brickPosList3 =
    [ Pos 500 800 4600 4650   --[0,1600]*[4000,4800]
    , Pos 100 350 4250 4300
    , Pos 850 1050 4200 4250
    , Pos 1300 1500 3975 4000 --[0,1600]*[4000,4800]
    , Pos 250 500 3835 3885   --[0,1600]*[3200,4000]
    , Pos 1000 1200 3650 3700
    , Pos 650 850 3535 3585
    , Pos 250 800 3175 3200
    , Pos 1100 1400 3315 3365 --[0,1600]*[3200,4000]
    , Pos 100 350 2900 2950   --[0,1600]*[2400,3200]
    , Pos 1200 1500 2935 2980
    , Pos 900 1000 2965 3000
    , Pos 1350 1500 2700 2765
    , Pos 900 1200 2700 2750
    , Pos 550 775 2600 2670
    , Pos 250 550 2515 2575   --[0,1600]*[2400,3200]
    , Pos 950 1300 2200 2225  --[0,1600]*[1600,2400]
    , Pos 950 1000 2225 2375
    , Pos 950 1300 2375 2400
    , Pos 1250 1300 2100 2200
    , Pos 1350 1500 1800 1865
    , Pos 600 650 1650 2000
    , Pos 900 950 1650 2000
    , Pos 0 600 2200 2250
    , Pos 350 600 1900 1950
    , Pos 100 300 1705 1755
    , Pos 600 1250 1590 1650  --[0,1600]*[1600,2400]
    , Pos 250 750 1200 1250   --[0,1600]*[800,1600]
    , Pos 1250 1500 1250 1300
    , Pos 900 1250 950 1000   --[0,1600]*[800,1600]
    , Pos 100 600 700 750     --[0,1600]*[0,800]
    , Pos 300 1300 300 350    --[0,1600]*[0,800]
    -- secret room
    , Pos -250 -100 2530 2580
    , Pos -400 -250 2610 2660
    , Pos -550 -400 2690 2740
    , Pos -700 -550 2770 2820
    , Pos -850 -700 2850 2900
    , Pos -1000 -850 2930 2980
    , Pos -1150 -1000 3010 3060
    , Pos -1300 -1150 3090 3140
    , Pos -1500 -1200 3500 3550
    , Pos -1200 -900 3700 3750
    , Pos -600 -100 3800 3850     
    , Pos -1600 0 1900 1950 
    , Pos -1500 -100 3900 3950
    , Pos -300 0 2200 2250 
    ]

brickWallList3 = 
    [ Pos 0 100 0 2000
    , Pos 0 100 2200 4800
    , Pos 100 1500 4735 4800
    , Pos 1500 1600 0 4800 
    , Pos -1600 -1500 2000 4000
 
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
    , Pos 650 750 1490 1590
    , Pos 800 900 1490 1590
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