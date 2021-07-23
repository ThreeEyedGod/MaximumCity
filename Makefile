all:
	@mkdir -p build
	@rm -rf ./build/*
	@stack clean --docker
	#stack build --docker
	@stack test --docker
	@cp `stack --docker path --local-install-root`/bin/bootstrap build
	@cp IP2LOCATION-LITE-DB11.IPV6.BIN.ZIP build 
	@cd build && unzip IP2LOCATION-LITE-DB11.IPV6.BIN.ZIP && cd ..
	@cd build && zip function.zip bootstrap IP2LOCATION-LITE-DB11.IPV6.BIN && rm bootstrap && rm IP2LOCATION-LITE-DB11.IPV6.* && cd ..
	@cd build && cp function.zip ../ && cd ..

test:
	@stack test
