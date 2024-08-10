.PHONY: init install_dependencies clean lint test verify_git_status update_version tag_version update_changelog build all

# Variables
SCRIPT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
PYPROJECT_FILE := $(SCRIPT_DIR)/pyproject.toml
VENV_DIR := $(SCRIPT_DIR)/.venv
CHANGELOG_FILE := $(SCRIPT_DIR)/CHANGELOG.md

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

clean:
	rm -fr build dist .ruff_cache

lint:
	@echo "Checking code style with ruff..."
	@set -e; \
	$(VENV_DIR)/bin/ruff check $(SCRIPT_DIR)

test:
	@echo "Running tests..."
	@. $(VENV_DIR)/bin/activate && cd $(SCRIPT_DIR)/tests && python -m unittest discover -p "test_*.py"

verify_git_status:
	@if ! git diff --cached --quiet; then \
		echo "Error: There are already staged files. Please commit or unstage them first."; \
		exit 1; \
	fi

	@if [ -z "$$(git log --pretty=format:'%s' $$(git describe --tags --abbrev=0)..HEAD)" ]; then \
		echo "Error: No commits to include in the changelog."; \
		exit 1; \
	fi

# Increment the version in pyproject.toml
update_version:
	@echo "Incrementing version..."
	@current_version=$$(grep 'version = ' $(PYPROJECT_FILE) | sed -E 's/version = "([0-9]+)\.([0-9]+)\.([0-9]+)"/\1.\2.\3/'); \
	major=$$(echo $$current_version | awk -F. '{print $$1}'); \
	minor=$$(echo $$current_version | awk -F. '{print $$2}'); \
	patch=$$(echo $$current_version | awk -F. '{print $$3}'); \
	new_patch=$$((patch + 1)); \
	new_version=$$major.$$minor.$$new_patch; \
	sed -i.bak -E "s/version = \"[0-9]+\\.[0-9]+\\.[0-9]+\"/version = \"$$new_version\"/" $(PYPROJECT_FILE); \
	rm -f $(PYPROJECT_FILE).bak;

# Add a git tag with the current version from pyproject.toml
tag_version:
	@current_version=$$(grep 'version = ' $(PYPROJECT_FILE) | sed -E 's/version = "([0-9]+)\.([0-9]+)\.([0-9]+)"/\1.\2.\3/'); \
	git tag -a v$$current_version -m "v$$current_version"

# Update the CHANGELOG.md file with the latest changes
update_changelog:
	@echo "Updating CHANGELOG.md..."
	@current_version=$$(grep 'version = ' $(PYPROJECT_FILE) | sed -E 's/version = "([0-9]+)\.([0-9]+)\.([0-9]+)"/\1.\2.\3/'); \
	echo "## v$$current_version ($$(date +'%Y-%m-%d'))\n" >> $(CHANGELOG_FILE); \
	git log --pretty=format:"- %s" $$(git describe --tags --abbrev=0 @^)..HEAD >> $(CHANGELOG_FILE); \
	echo "\n" >> $(CHANGELOG_FILE);

commit_core_files:
	@current_version=$$(grep 'version = ' $(PYPROJECT_FILE) | sed -E 's/version = "([0-9]+)\.([0-9]+)\.([0-9]+)"/\1.\2.\3/'); \
	echo "Committing docs and config files..."; \
	git add $(PYPROJECT_FILE) $(CHANGELOG_FILE); \
	git commit -m "Update docs and config files"; \

build:
	@echo "Building package..."
	make clean
	make lint 
	make test
	make verify_git_status
	make update_version
	make update_changelog
	make commit_core_files
	make tag_version
	@. $(VENV_DIR)/bin/activate && poetry build

#publish:
#	make build
#	pip3 install 'twine>=1.5.0'
#	twine upload dist/*
#	make clean
