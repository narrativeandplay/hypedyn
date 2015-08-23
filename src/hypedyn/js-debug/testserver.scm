; test server

; start the server
(gnu.kawa.servlet.KawaHttpHandler:addAutoHandler "/" ".")
(gnu.kawa.servlet.KawaHttpHandler:startServer 8888)
(format #t "running...~%~!")