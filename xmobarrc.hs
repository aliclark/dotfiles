
Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
       , bgColor = "black"
       , fgColor = "grey"
       , position = TopW L 95
       , lowerOnStart = True
       , commands = [Run Cpu ["-L","10","-H","50","-l","green","--normal","orange","--high","red"] 10
                    , Run Network "eth1" ["-L","0","-H","32","-l", "green", "--normal","orange","--high","red"] 40
                    , Run Memory ["-t","Mem: <usedratio>","-L","25","-H","50","-l","green","--normal","orange","--high","red"] 10
                    , Run StdinReader
                    , Run Battery ["left","-L","10","-H","50","-l","red","--normal","orange","--high","green"] 600
                    , Run Com "/usr/local/bin/chkgmail" [] "mail" 6000
                    , Run Com "/usr/local/bin/chkdate" [] "chkdate" 10
                    , Run Network "eth0" ["-L","0","-H","32","-l", "green", "--normal","orange","--high","red"] 40
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% %eth1% %memory% %battery% Date: %chkdate% Mail: %mail% "
       }

