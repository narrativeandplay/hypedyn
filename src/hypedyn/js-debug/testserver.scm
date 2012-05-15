; test server

; start the server
(gnu.kawa.servlet.KawaHttpHandler:addAutoHandler "/" ".")
(gnu.kawa.servlet.KawaHttpHandler:startServer 8888)
(display "running\n")