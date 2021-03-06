
Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
       , bgColor = "black"
       , fgColor = "grey"
       , position = TopW L 95
       , lowerOnStart = True
       , commands = [Run Cpu ["-L","10","-H","50","-l","green","--normal","orange","--high","red"] 10
                    , Run Network "eth0" ["-L","0","-H","32","-l", "green", "--normal","orange","--high","red"] 40
                    , Run Network "eth1" ["-L","0","-H","32","-l", "green", "--normal","orange","--high","red"] 40
                    , Run Memory ["-t","Mem: <usedratio>%","-L","25","-H","50","-l","green","--normal","orange","--high","red"] 10
                    , Run StdinReader
                    -- , Run Battery ["left","-L","10","-H","50","-l","red","--normal","orange","--high","green"] 600
                    , Run BatteryP ["BAT0"]
                      ["-t", "Batt: <left>%",
                       "-L", "10", "-H", "50", "-p", "3",
                       "--", "-O", "", "-o", "",
                       "-L", "-15", "-H", "-5",
                       "-l", "red", "-m", "orange", "-h", "green"]
                      60
                    , Run Com "chkdate" [] "chkdate" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% %eth0% %eth1% %memory% %battery% Date: %chkdate% "
       }

