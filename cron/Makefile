.PHONY: all
all: work/index-anonymous.md

generated/PatreonInternalApi/Api.elm: src/openapi.yaml
	bunx elm-pages run ~/src/wolfadex/elm-open-api-cli/cli/src/Cli.elm ./src/openapi.yaml --effect-types dillonkearns/elm-pages.taskrecord

work/index-anonymous.md: generated/PatreonInternalApi/Api.elm
	bun start
