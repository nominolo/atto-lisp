.PHONY: test
test:
	stack test --stack-yaml=stack-lts2.yaml
	stack test --stack-yaml=stack-lts3.yaml
