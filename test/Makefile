POLYMLB   != realpath ../polymlb

SML       != ls sml/*.sml
REPOS     != ls repos/*.t
REPOS_DIR := repos/r

all: sml

sml: $(SML)

$(SML):
	echo 'use "test.sml"; use "$@";' | poly --error-exit > /dev/null

repos: $(REPOS)

# requires gmake
$(REPOS):
	[ -d "$(REPOS_DIR)/$(@F:.t=)" ] || { \
		git clone --depth 1 $$(cat $@) $(REPOS_DIR)/$(@F:.t=) && \
		cd $(REPOS_DIR)/$(@F:.t=) && \
		smlpkg sync && \
		git apply ../../$(@F:.t=.patch); \
	}
	$(MAKE) -C $(REPOS_DIR)/$(@F:.t=) MLCOMP=$(POLYMLB) clean all

clean:
	rm -rf $(REPOS_DIR)

.PHONY: $(SML) $(REPOS) all clean
