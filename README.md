################################################################################ 
## simple_tcp介绍
################################################################################ 
    simple_tcp提供了一对简单的tcp的server和client,
    他们封装了部分代码,以便用户快速完成工作(封装部分如连接建立和关闭,连接错误处理,进程与连接映射并处理IO,ssl细节等),
    他们提供了用户接口,以便用户自定义需求.
    (使用方法参考"xxx测试例子")
    (接口参数见"simple_tcp接口参数")


################################################################################ 
## tcp服务端测试例子
################################################################################# 
    1.在终端1, 启动tcp_server应用,建立监听 tcp服务
        $ cd simple_tcp                                         % 进入目录
        $ make
        $ erl -sname tcp_server -setcookie x -pa ebin +K true
        > tcp_server:start(). 
        ok
        > tcp_server:daemon(9999, []). 
        ok

	2.在终端2, 模拟客户端,向tcp应用发请求,会输出响应
	    $ echo -n "abc" | nc 127.0.0.1 9999
        abc
        

################################################################################ 
## tcp客户端(短连接)测试例子
################################################################################# 

    1.在终端1,模拟服务器收到请求后直接返回响应
	    $ echo -n "abc" | nc -l 9999

	2.在终端2, 客户端启动, 发请求
	    $ erl -sname tcp_client -setcookie x -pa ebin +K true
	    tcp_client:request("127.0.0.1", 9999, "123").
        > {ok,"abc"}



################################################################################ 
## 使用openssl生成证书(笔记者总结)
################################################################################# 
    进入目录   
        $ cd simple_tcp/certs/server/

    生成一份 CA 根证书
        $ openssl genrsa -des3 -out ca_private_key.pem 2048
            需要输入如:
                ...
                Enter pass phrase for ca_private_key.pem:12345678
                ...
        $ openssl req -new -x509 -sha1 -days 3650 -key ca_private_key.pem -out ca.crt
            需要输入如:
                ...
                Enter pass phrase for ca_private_key.pem:12345678
                Country Name (2 letter code) [XX]:CN
                State or Province Name (full name) []:.
                Locality Name (eg, city) [Default City]:.
                Organization Name (eg, company) [Default Company Ltd]:.
                Organizational Unit Name (eg, section) []:.
                Common Name (eg, your name or your server's hostname) []:.
                Email Address []:.
                ...

    生成公钥私钥
        $ openssl genrsa -out server_private_key.pem 2048                                                     # 生成私钥
        $ openssl rsa -pubout -in server_private_key.pem -out server_public_key.pem                           # 生成公钥

    颁发服务器证书
        $ openssl req -new -key server_private_key.pem -out server.csr
            需要输入如:
                ...
                ountry Name (2 letter code) [XX]:CN
                State or Province Name (full name) []:BeiJing
                Locality Name (eg, city) [Default City]:.
                Organization Name (eg, company) [Default Company Ltd]:.
                Organizational Unit Name (eg, section) []:.
                Common Name (eg, your name or your server's hostname) []:.
                Email Address []:.

                Please enter the following 'extra' attributes
                to be sent with your certificate request
                A challenge password []:.
                An optional company name []:.
                ...
        $ openssl x509 -req -sha1 -days 3650 -in server.csr -CA ca.crt -CAkey ca_private_key.pem -CAcreateserial -out server.crt



################################################################################ 
## ssl服务端测试例子
################################################################################# 
    1.在终端1, 启动tcp_server应用,建立监听 tcp服务
        $ cd simple_tcp                                         # 进入目录
        $ make
        $ erl -sname tcp_server -setcookie x -pa ebin +K true
        > tcp_server:start(). 
        ok
        > tcp_server:daemon(9999, [{is_ssl, true}]). 
        ok

	2.在终端2, 模拟客户端,向tcp应用发请求,会输出响应
        $ openssl s_client -connect 127.0.0.1:9999
        ...
        abc # 输入
        abc # 输出

	3.在终端2, 模拟客户端,用同一个sessionid向tcp应用连接5次(测试会话恢复)
        $ openssl s_client -connect 127.0.0.1:9999 -sess_out sess.pem                   # 第1次连接,把session信息写入文件
        ...
        Session-ID: 44A59CE792F1F3640CFF7EC7D7EC0CE134DB736D3C998F3D934CC50C91A660F2        # 第1次的sessionid
        ...
        Q   # 退出

        $ openssl s_client -connect 127.0.0.1:9999 -sess_in sess.pem                    # 第2次连接,从文件读入session信息,从而恢复会话
        ...
        Session-ID: 44A59CE792F1F3640CFF7EC7D7EC0CE134DB736D3C998F3D934CC50C91A660F2        # 第2次的sessionid和第1次的一样,可见恢复会话成功
        ...


################################################################################ 
## ssl客户端测试例子
################################################################################# 

    1.在终端1,模拟服务器收到请求后直接返回响应
        $ cd simple_tcp/certs/server/                                                   # 进入证书目录
        $ openssl s_server -accept 9999 -cert server.crt -key server_private_key.pem
        ...

	2.在终端2, 客户端启动, 发请求
	    $ erl -sname tcp_client -setcookie x -pa ebin +K true
	    tcp_client:ssl_request({127,0,0,1}, 9999, "123\n").

    3.在终端3, 收到请求,返回响应(一个回车)
        ...
        123                 # 输出请求
        abc                 # 输入响应 
        ...

	4.在终端2, 返回响应
        {ok,"abc\n"}

	5.在终端2, 客户端启动, 发请求(测试会话恢复)
	    tcp_client:ssl_request({127,0,0,1}, 9999, "123\n").

    6.在终端3, 打印中看到恢复了会话
        ...
        Reused session-id
        ...


################################################################################ 
## simple_tcp接口参数
################################################################################# 
    服务器
        (使用方法见下文测试例子,或详见tcp_server模块接口)
        接口参数提供了用户自定义选项如:
            监听端口:   tcp_server:daemon(9999, []). 
            请求处理函数:   tcp_server:daemon(9999, [{msg_handle, {tcp_server_util, echo}}]).
            长或短连接: tcp_server:daemon(9999, [{connect_type, long}]). 或 tcp_server:daemon(9991, [{connect_type, short}]). 
            用ssl(默认支持恢复会话): tcp_server:daemon(9999, [{is_ssl, true}]). 
            用ssl时的证书和私钥: tcp_server:daemon(9999, [{is_ssl, true}, {certfile, "certs/server/server.crt"}, {keyfile, "certs/server/server_private_key.pem"}]). 

    客户端
        (使用方法见下文测试例子,或详见tcp_client模块的接口)
        接口参数提供了用户自定义选项如:
            连接的地址,端口,发送的消息: tcp_client:request("127.0.0.1", 9999, "123").
            用ssl(默认支持恢复会话): tcp_client:ssl_request({127,0,0,1}, 9999, "123").
            发送超时时间(单位毫秒): tcp_client:request("127.0.0.1", 9999, 5000, "123").

