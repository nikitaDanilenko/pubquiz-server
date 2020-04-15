binPath=./bin
serviceName=pubquiz-service

upgrade:
	stack install pubquiz-server --local-bin-path $(binPath)
	sudo systemctl restart $(serviceName)