.PHONY: test lint version build all help

# Variables
SCRIPT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
PYPROJECT_FILE := $(SCRIPT_DIR)/pyproject.toml
VENV_DIR := $(SCRIPT_DIR)/.venv

# Install dependencies and set up the environment
init:
	@echo "Installing dependencies..."
	@if command -v pyenv >/dev/null 2>&1; then \
		pyenv install -s 3.9.0 && pyenv local 3.9.0; \
		poetry env use $(pyenv which python); \
	else \
		echo "pyenv not found, using system Python."; \
	fi
	make install_dependencies

install_dependencies:
	@echo "Installing dependencies..."
	@poetry install

lint:
	@echo "Checking code style with ruff..."
	@. $(VENV_DIR)/bin/activate && ruff check $(SCRIPT_DIR)

test:
	@echo "Running tests..."
	@. $(VENV_DIR)/bin/activate && cd $(SCRIPT_DIR)/tests && python -m unittest discover -p "test_*.py"

# Increment the version in pyproject.toml
version:
	@echo "Incrementing version..."
	@current_version=$$(grep 'version = ' $(PYPROJECT_FILE) | sed -E 's/version = "([0-9]+)\.([0-9]+)\.([0-9]+)"/\1.\2.\3/'); \
	major=$$(echo $$current_version | awk -F. '{print $$1}'); \
	minor=$$(echo $$current_version | awk -F. '{print $$2}'); \
	patch=$$(echo $$current_version | awk -F. '{print $$3}'); \
	new_patch=$$((patch + 1)); \
	new_version=$$major.$$minor.$$new_patch; \
	sed -i.bak -E "s/version = \"[0-9]+\\.[0-9]+\\.[0-9]+\"/version = \"$$new_version\"/" $(PYPROJECT_FILE); \
	rm -f $(PYPROJECT_FILE).bak; \
	git add $(PYPROJECT_FILE); \
	git commit -m "Version incremented to $$new_version"

clean:
	rm -fr build dist .ruff_cache

build:
	@echo "Building package..."
	make clean
	make lint 
	make test
	make version
	@. $(VENV_DIR)/bin/activate && poetry build

#publish:
#	make build
#	pip3 install 'twine>=1.5.0'
#	twine upload dist/*
#	make clean

# Display help message
help:
	@echo "Usage: make [install | test | lint | version | all | help]"
	@echo ""
	@echo "Options:"
	@echo "  init      Install dependencies and set up the environment"
	@echo "  test      Run tests"
	@echo "  lint      Check code style with ruff"
	@echo "  version   Increment the version in pyproject.toml"
	@echo "  clean     Clean up build files"
	@echo "  build     Build package"
	@echo "  all       Run all targets"
	@echo "  help      Display this help message"
