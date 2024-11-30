new:
ifdef day
	@echo 'Making module for day $(day)'
	@cp days/Day0.hs days/Day$(day).hs
	@sed -i '' 's/Day0/Day$(day)/g' days/Day1.hs
	@mkdir data/day$(day)
	@touch data/day$(day)/data1.txt
	@touch data/day$(day)/data2.txt
	@mkdir tests/day$(day)
	@cp -R tests/day0/ tests/day$(day)
	@sed -i '' 's/Day0/Day$(day)/g' tests/day$(day)/Main.hs
	@sed -i '' 's/library_target/library_target\n        Day$(day),/' advent-of-code2024.cabal
	@echo '\ntest-suite day$(day)\n    default-language: GHC2021\n    type: exitcode-stdio-1.0\n    hs-source-dirs:   tests/day$(day)\n    main-is:          Main.hs\n    build-depends:\n        advent-of-code2024,\n        base ^>=4.17.2.1,\n        hspec ^>=2.11.9' >> advent-of-code2024.cabal
	@sed -i '' 's/getDay :: String -> Runner/getDay :: String -> Runner\ngetDay "$(day)" = Day$(day).run/' app/Main.hs
	@sed -i '' 's/import Day0/import Day0\nimport Day$(day)/' app/Main.hs
else
	@echo 'Missing argument day=<n>'
endif

run:
ifdef day
ifdef part
	cabal run advent-of-code2024 -- $(day) -p $(part)
else
	cabal run advent-of-code2024 -- $(day)
endif
else
	@echo 'Missing argument day=<n>'
endif

test:
ifdef day
	cabal test day$(day)
else
	@echo 'Missing argument day=<n>'
endif