.PHONY: all test clean

fmt:
	find . -name '*.hs' -type f -exec ormolu --mode inplace {} \;
	find . -name '*.nix' -exec nixfmt {} \;
	-statix check
	-deadnix -f
spin-up-containers-local:
	docker-compose -f resources/docker/local-compose.yaml up -d
dev: fmt
	watchexec -r -e hs,cabal nix run --print-build-logs . -- "./resources/config/run-local.toml"
run-production:
	nix run . -- "./resources/config/run-production.toml"
test:
	nix run .#test
push-cache:
	nix path-info --recursive | cachix push wikimusic-api
	nix flake --extra-experimental-features 'nix-command flakes' \
		--accept-flake-config archive --json | jq --raw-output '.path, (.inputs | to_entries [] .value.path)' | cachix push wikimusic-api
push-cache-arm:
	nix --system aarch64-linux path-info --recursive | cachix push wikimusic-api
	nix --system aarch64-linux flake --extra-experimental-features 'nix-command flakes' \
		--accept-flake-config archive --json | jq --raw-output '.path, (.inputs | to_entries [] .value.path)' | cachix push wikimusic-api

grab-db-backup:
	bash ./resources/scripts/grab-db-backup.bash
cabal-release:
	cabal sdist -o .

build:
	nix build --extra-experimental-features 'nix-command flakes' --accept-flake-config --print-build-logs --system x86_64-linux
build-arm:
	nix build --extra-experimental-features 'nix-command flakes' --accept-flake-config --print-build-logs --system aarch64-linux
