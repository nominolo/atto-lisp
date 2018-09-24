STACK_PREVIOUS=stack-lts11.yaml
STACK_LATEST=stack-lts12.yaml

.PHONY: test
test:
	stack test --stack-yaml=$(STACK_PREVIOUS)
	stack test --stack-yaml=$(STACK_LATEST)

release:
	stack sdist --stack-yaml=$(STACK_LATEST)
	@version=`stack --stack-yaml=$(STACK_LATEST) ls dependencies | \
		grep ^atto-lisp | cut -d' ' -f2` && \
		git tag -a -m "Version $$version" v$$version
	@echo "Please upload the distfile from above"

	
