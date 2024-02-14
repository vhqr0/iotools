(require
  dash *)

(import
  unittest [IsolatedAsyncioTestCase]
  dash *
  dash.strtools :as s
  ssl
  asyncio
  aiohttp
  aiohttp.web :as web
  iotools *
  iotools.proto.ssl *
  iotools.proto.http *
  iotools.proto.ws *
  iotools.proto.socks5 *)

;; openssl req -new -x509 -days 365 -nodes -out cert.pem -keyout key.pem
(setv COMMONNAME "localhost"
      CERTFILE   "tests_resources/cert.pem"
      KEYFILE    "tests_resources/key.pem")

(defn client-context []
  (ssl.create-default-context :cafile CERTFILE))

(defn server-context []
  (doto (ssl.create-default-context ssl.Purpose.CLIENT-AUTH)
        (.load-cert-chain :certfile CERTFILE :keyfile KEYFILE)))



(defn/a run-iotools-https-server [[host "localhost"] [port 4433]]
  (defn/a handler [reader writer]
    (with/a [tcp-stream (AsyncTCPStream :reader reader :writer writer)]
      (with/a [ssl-stream (await (.handshake (AsyncSSLAcceptor :ssl-context (server-context)) tcp-stream))]
        (let [#(method path version headers) (await (.read-loop ssl-stream HTTPRequest.read))]
          (await (.write ssl-stream (HTTPResponse.pack #("HTTP/1.1" "200" "OK" {"Transfer-Encoding" "chunked"}))))
          (with/a [chunked-stream (AsyncHTTPChunkedStream :next-layer ssl-stream)]
            (await (.write chunked-stream (s.encode method)))
            (await (.flush chunked-stream))
            (await (.write chunked-stream (s.encode path)))
            (await (.flush chunked-stream))
            (await (.write chunked-stream (s.encode version)))
            (await (.flush chunked-stream)))))))
  (with/a [server (await (asyncio.start-server handler host port))]
    (print "start https server ...")
    (try
      (await (.serve-forever server))
      (except [_ asyncio.CancelledError]
        (print "shutdown https server ...")
        (raise))
      (except [e Exception]
        (print "error while serving:" e)
        (raise)))))

(setv proxy-tasks (set))

(defn/a run-iotools-proxy-server [[host "localhost"] [port 10800]]
  (defn/a stream-copy [from to]
    (for [:async data from]
      (await (.write to data))
      (await (.flush to))))
  (defn/a handler [reader writer]
    (with/a [tcp-stream (AsyncTCPStream :reader reader :writer writer)]
      (let [acceptor (AsyncAutoProxyAcceptor)]
        (with/a [in-stream (await (.handshake acceptor tcp-stream))]
          (print "proxy server connect to" acceptor.host acceptor.port)
          (with/a [out-stream (await (AsyncTCPStream.open acceptor.host acceptor.port))]
            (let [tasks [(asyncio.create-task (stream-copy in-stream out-stream))
                         (asyncio.create-task (stream-copy out-stream in-stream))]]
              (--each tasks
                      (.add proxy-tasks it)
                      (.add-done-callback it proxy-tasks.discard))
              (try
                (await (asyncio.gather #* tasks))
                (except [Exception]))
              (--each tasks (.cancel it))))))))
  (with/a [server (await (asyncio.start-server handler host port))]
    (print "start proxy server ...")
    (try
      (await (.serve-forever server))
      (except [_ asyncio.CancelledError]
        (print "shutdown proxy server ...")
        (raise))
      (except [e Exception]
        (print "error while serving:" e)
        (raise)))))



(defn/a run-aiohttp-https-client
  [[host "localhost"] [port 4433] [path "/"] [proxy-proto "http"]
   [proxy-host "localhost"] [proxy-port 10800]]
  (with/a [session (aiohttp.ClientSession)]
    (with/a [response (.get session (s.format "https://{}:{}{}" host port path)
                            :ssl (client-context)
                            :proxy (s.format "{}://{}:{}" proxy-proto proxy-host proxy-port))]
      (let [datas (list)]
        (for [:async data (.iter-chunked response.content 10)]
          (.append datas data))
        (s.concatb-in datas)))))

(defn/a run-iotools-https-client
  [[host "localhost"] [port 4433] [path "/"] [proxy-proto "http"]
   [proxy-host "localhost"] [proxy-port 10800]]
  (with/a [tcp-stream (await (AsyncTCPStream.open proxy-host proxy-port))]
    (let [proxy-connector ((match proxy-proto
                                  "http" AsyncHTTPProxyConnector
                                  "socks5h" AsyncSocks5Connector
                                  _ (raise ValueError))
                            :host host :port port)]
      (with/a [proxy-stream (await (.handshake proxy-connector tcp-stream))]
        (with/a [ssl-stream (await (.handshake (AsyncSSLConnector :ssl-context (client-context) :server-hostname host) proxy-stream))]
          (await (.write ssl-stream (HTTPRequest.pack #("GET" path "HTTP/1.1" {"Host" "localhost"}))))
          (await (.flush ssl-stream))
          (await (.read-loop ssl-stream HTTPResponse.read))
          (with/a [chunked-stream (AsyncHTTPChunkedStream :next-layer ssl-stream)]
            (let [datas (list)]
              (for [:async data chunked-stream]
                (.append datas data))
              (s.concatb-in datas))))))))



(defn/a run-aiohttp-ws-server [[host "localhost"] [port 4433] [path "/"]]
  (defn/a handler [request]
    (let [ws (web.WebSocketResponse)]
      (await (.prepare ws request))
      (let [message (await (anext ws))]
        (print "ws server recv" message.data)
        (await (.send-bytes ws message.data))
        (await (.close ws)))
      ws))
  (let [app (web.Application)]
    (.add-routes app [(web.get path handler)])
    (print "start ws server ...")
    (try
      (await (web._run-app app :port port))
      (except [_ asyncio.CancelledError]
        (print "shutdown ws server ...")
        (raise))
      (except [e Exception]
        (print "error while serving:" e)
        (raise)))))

(defn/a run-aiohttp-ws-client [[data b"hello"] [host "localhost"] [port 4433] [path "/"]]
  (with/a [session (aiohttp.ClientSession)]
    (with/a [ws (.ws-connect session (s.format "http://{}:{}{}" host port path))]
      (await (.send-bytes ws data))
      (let [message (await (anext ws))]
        (await (.close ws))
        message.data))))



(defn/a run-iotools-ws-server [[host "localhost"] [port 4433] [path "/"]]
  (defn/a handler [reader writer]
    (with/a [tcp-stream (AsyncTCPStream :reader reader :writer writer)]
      (with/a [ws-stream (await (.handshake (AsyncWSAcceptor) tcp-stream))]
        (let [data (await (.read ws-stream))]
          (print "ws server recv" data)
          (await (.write ws-stream data))
          (await (.flush ws-stream))))))
  (with/a [server (await (asyncio.start-server handler host port))]
    (print "start ws server ...")
    (try
      (await (.serve-forever server))
      (except [_ asyncio.CancelledError]
        (print "shutdown ws server ...")
        (raise))
      (except [e Exception]
        (print "error while serving:" e)
        (raise)))))

(defn/a run-iotools-ws-client [[data b"hello"] [host "localhost"] [port 4433] [path "/"]]
  (with/a [tcp-stream (await (AsyncTCPStream.open :host host :port port))]
    (with/a [ws-stream (await (.handshake (AsyncWSConnector :host host :path path) tcp-stream))]
      (await (.write ws-stream data))
      (await (.flush ws-stream))
      (let [data (await (.read ws-stream))]
        data))))



(defclass TestHTTPS [IsolatedAsyncioTestCase]
  (setv port 4433
        proxy-port 10800)

  (defn/a asyncSetUp [self]
    (let [tasks [(asyncio.create-task (run-iotools-https-server :port self.port))
                 (asyncio.create-task (run-iotools-proxy-server :port self.proxy-port))]]
      (setv self.tasks tasks)))

  (defn/a asyncTearDown [self]
    (--each self.tasks (.cancel it)))

  (defn/a test-aiohttp-client [self]
    (.assertEqual self (await (run-aiohttp-https-client :port self.port :proxy-proto "http"    :proxy-port self.proxy-port)) b"GET/HTTP/1.1")
    (.assertEqual self (await (run-aiohttp-https-client :port self.port :proxy-proto "socks5h" :proxy-port self.proxy-port)) b"GET/HTTP/1.1"))

  (defn/a test-iotools-client [self]
    (.assertEqual self (await (run-iotools-https-client :port self.port :proxy-proto "http"    :proxy-port self.proxy-port)) b"GET/HTTP/1.1")
    (.assertEqual self (await (run-iotools-https-client :port self.port :proxy-proto "socks5h" :proxy-port self.proxy-port)) b"GET/HTTP/1.1")))

(defclass TestWSMixin []
  (setv port 4433)

  (defn/a asyncTearDown [self]
    (.cancel self.task))

  (defn/a test-aiohttp-client [self]
    (.assertEqual self (await (run-aiohttp-ws-client :data b"hello" :port self.port)) b"hello"))

  (defn/a test-iotools-client [self]
    (.assertEqual self (await (run-iotools-ws-client :data b"hello" :port self.port)) b"hello")))

(defclass TestWSClient [TestWSMixin IsolatedAsyncioTestCase]
  (defn/a asyncSetUp [self]
    (let [task (asyncio.create-task (run-aiohttp-ws-server :port self.port))]
      (setv self.task task))))

(defclass TestWSServer [TestWSMixin IsolatedAsyncioTestCase]
  (defn/a asyncSetUp [self]
    (let [task (asyncio.create-task (run-iotools-ws-server :port self.port))]
      (setv self.task task))))

(export
  :objects [TestHTTPS TestWSClient TestWSServer])
