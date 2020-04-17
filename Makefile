# 默认
all: compile

# 编译
compile:
	@rm -rf ebin/
	@mkdir ebin/
	@cp -f src/*/*.app.src ebin/  
	@mv ebin/tcp_server.app.src ebin/tcp_server.app
	@erl -make -smp disable 

# 清理
clean:
	@rm -rf ebin/*
	@rm -rf tags
	@rm -rf TAGS
	@rm -rf erl_crash.dump

# 服务端测试
test_server: compile
	@echo " 测试需要执行步骤如下"
	@echo " "
	@echo " 1.在终端1, 启动tcp服务"
	@echo " 	erl -sname tcp_server -setcookie x -pa ebin +K true"
	@echo " "
	@echo " 3.在终端2, 模拟客户端,向tcp应用发请求,收响应即response.txt"
	@echo " 	echo -n \"request\" | nc 127.0.0.1 9999 > response.txt"
	@echo " "
	
# 客户端测试
test_client: compile
	@echo " 测试需要执行步骤如下"
	@echo " 1.在终端1, 模拟tcp服务启动"
	@echo " 	nc -l 9999"
	@echo " "
	@echo " 2.在终端2, 客户端启动, 发请求"
	@echo " 	erl -sname tcp_client -setcookie x -pa ebin +K true"
	@echo " 	tcp_client:request(\"127.0.0.1\", 9999, \"request_msg\\n\")."
	@echo " "
	@echo " 3.在终端1, 模拟tcp服务收请求,发响应"
	@echo " 	会看到打印request_msg,然后输入response_msg回车"
	@echo " "
	@echo " 4.在终端2, 客户端收响应"
	@echo " 	会看到打印{ok,\"response_msg\\n\"}"
	
