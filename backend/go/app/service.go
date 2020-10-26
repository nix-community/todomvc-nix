package app

import (
	"context"
	"errors"
	"fmt"
)

// Service is a simple CRUD interface for user profiles.
type Service interface {
	GetTodos(ctx context.Context) ([]Todo, error)
	GetTodo(ctx context.Context, id int) (Todo, error)
	InsertTodo(ctx context.Context, t Todo) (Todo, error)
	DeleteTodos(ctx context.Context) error
	DeleteTodo(ctx context.Context, id int) error
	UpdateTodo(ctx context.Context, id int, t Todo) (Todo, error)
	Clear() error
}

var (
	ErrInconsistentIDs = errors.New("inconsistent IDs")
	ErrAlreadyExists   = errors.New("already exists")
	ErrNotFound        = errors.New("not found")
)

type serviceImpl struct {
	repository Repository
	cfg        *Config
}

type Todo struct {
	ID        *int    `json:"id"`
	Title     *string `json:"title"`
	Completed *bool   `json:"completed"`
	Order     *int    `json:"order"`
	URL       string  `json:"url"`
}

func NewService(repository Repository, cfg *Config) Service {
	return &serviceImpl{repository: repository, cfg: cfg}
}

var blank = Todo{}

func (s *serviceImpl) InsertTodo(ctx context.Context, t Todo) (Todo, error) {
	if t.Completed == nil {
		var b bool
		t.Completed = &b
	}

	if t.Order == nil {
		var i int
		t.Order = &i
	}

	if t.Title == nil {
		var s string
		t.Title = &s
	}

	todo, err := s.repository.Save(ctx, t)
	if err != nil {
		return blank, err
	}

	return s.addURL(todo), nil
}

func (s *serviceImpl) GetTodo(ctx context.Context, id int) (Todo, error) {
	todo, err := s.repository.Get(ctx, id)
	if err != nil {
		return blank, err
	}

	return s.addURL(todo), nil
}

func (s *serviceImpl) UpdateTodo(ctx context.Context, id int, t Todo) (Todo, error) {
	todo, err := s.repository.Update(ctx, id, t)
	if err != nil {
		return blank, err
	}

	return s.addURL(todo), nil
}

func (s *serviceImpl) DeleteTodo(ctx context.Context, id int) error {
	return s.repository.Delete(ctx, id)
}

func (s *serviceImpl) GetTodos(ctx context.Context) ([]Todo, error) {
	todos, err := s.repository.GetAll(ctx)
	if err != nil {
		return nil, err
	}

	if len(todos) == 0 {
		todos = []Todo{}
	}

	for i := range todos {
		todos[i] = s.addURL(todos[i])
	}

	return todos, err
}

func (s *serviceImpl) DeleteTodos(ctx context.Context) error {
	return s.repository.DeleteAll(ctx)
}

func (s *serviceImpl) addURL(todo Todo) Todo {
	id := *todo.ID

	todo.URL = fmt.Sprintf("%s/%d", s.cfg.URL, id)

	return todo
}

func (s *serviceImpl) Clear() error {
	return s.repository.Drop()
}
