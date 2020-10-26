package app

import "github.com/kelseyhightower/envconfig"

type Config struct {
	ServerPort       string `envconfig:"port" default:"8182"`
	DatabaseHost     string `default:"localhost"`
	DatabasePort     string `default:"5432"`
	DatabaseUsername string `default:"todomvc_dbuser"`
	DatabasePassword string `default:"todomvc_dbpass"`
	DatabaseSchema   string `default:"todomvc_db"`
	URL              string `default:"http://localhost:8182"`
}

func CreateConfig() (*Config, error) {
	c := &Config{}

	err := envconfig.Process("todo", c)
	if err != nil {
		return nil, err
	}

	return c, nil
}
