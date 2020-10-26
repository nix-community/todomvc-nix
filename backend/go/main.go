package main

import (
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/go-co-op/gocron"
	"github.com/go-kit/kit/log"
	"github.com/numtide/todomvc-nix/app"
)

func main() {
	logger := log.NewLogfmtLogger(os.Stdout)
	logger = log.With(logger, "ts", log.DefaultTimestampUTC)
	logger = log.With(logger, "caller", log.DefaultCaller)

	config, err := app.CreateConfig()
	if err != nil {
		logger.Log("config", err)
	}

	r, err := app.NewPostgresRepository(config)
	if err != nil {
		logger.Log("repository", err)
		os.Exit(1)
	}

	s := app.NewService(r, config)
	s = app.LoggingMiddleware(logger)(s)
	initializeCron(s)

	h := app.MakeHTTPHandler(s, log.With(logger, "component", "HTTP"))

	errs := make(chan error)
	go func() {
		c := make(chan os.Signal, 1)
		signal.Notify(c, syscall.SIGINT, syscall.SIGTERM)
		errs <- fmt.Errorf("%s", <-c)
	}()

	go func() {
		logger.Log("transport", "HTTP", "addr", config.ServerPort)
		errs <- http.ListenAndServe(fmt.Sprintf(":%s", config.ServerPort), h)
	}()

	logger.Log("exit", <-errs)
}

func initializeCron(s app.Service) {
	cron := gocron.NewScheduler(time.UTC)
	cron.Every(20).Minutes().Do(s.Clear)
	cron.StartAsync()
}
