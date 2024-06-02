all: rebar3
	$(REBAR) compile

run: rebar3
	$(REBAR) shell
	
publish: rebar3
	$(REBAR) hex publish

REBAR3_MK_VERSION := master
REBAR3_MK_URL := https://raw.githubusercontent.com/choptastic/rebar3.mk/$(REBAR3_MK_VERSION)/rebar3.mk

# Download rebar3.mk if it doesn't exist
rebar3.mk:
	@while true; do \
		echo "!!! This project wants to download rebar3.mk (version: $(REBAR3_MK_VERSION)) from: "; \
		echo "!!! $(REBAR3_MK_URL)"; \
		echo "!!! This is to assist with the build process"; \
		read -r -p "!!! Proceed? [Y/N] " yn; \
		case $$yn in \
			[Yy]*) \
				echo "Downloading rebar3..."; \
				curl -O $(REBAR3_MK_URL) || { echo "Failed to download file"; exit 1; }; \
				echo "Downloaded rebar3.mk"; \
				break;; \
			[Nn]*) \
				echo "Download aborted. This make is about to crash..."; \
				exit 1;; \
			*) \
				echo "Please answer yes or no.";; \
		esac; \
	done;

include rebar3.mk

