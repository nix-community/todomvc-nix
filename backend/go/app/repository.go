package app

import (
	"context"
	"fmt"

	"gorm.io/driver/postgres"
	"gorm.io/gorm"
)

type Repository interface {
	GetAll(context.Context) ([]Todo, error)
	Get(ctx context.Context, id int) (Todo, error)
	Save(ctx context.Context, t Todo) (Todo, error)
	DeleteAll(context.Context) error
	Delete(ctx context.Context, id int) error
	Update(ctx context.Context, id int, t Todo) (Todo, error)
	Drop() error
}

type postgresRepository struct {
	db *gorm.DB
}

func NewPostgresRepository(c *Config) (*postgresRepository, error) {
	connectionString := fmt.Sprintf("host=%s port=%s user=%s dbname=%s password=%s sslmode=disable", c.DatabaseHost, c.DatabasePort, c.DatabaseUsername, c.DatabaseSchema, c.DatabasePassword)

	db, err := gorm.Open(postgres.Open(connectionString), &gorm.Config{
		SkipDefaultTransaction: true,
		DisableForeignKeyConstraintWhenMigrating: true,
	})
	if err != nil {
		return nil, err
	}

	p := &postgresRepository{db: db}

	db.AutoMigrate(&todo{})

	return p, nil
}

func (p *postgresRepository) GetAll(ctx context.Context) ([]Todo, error) {
	var t []todo

	err := p.db.WithContext(ctx).Order(`"orderx" asc`).Find(&t).Error
	if err != nil {
		return nil, err
	}

	r := make([]Todo, len(t))
	for i, v := range t {
		r[i] = toDTO(v)
	}

	return r, nil
}

func (p *postgresRepository) Get(ctx context.Context, id int) (Todo, error) {
	var t todo

	err := p.db.WithContext(ctx).First(&t, id).Error
	if err != nil {
		return blank, err
	}

	return toDTO(t), nil
}

func (p *postgresRepository) Save(ctx context.Context, t Todo) (Todo, error) {
	m := toModel(t)

	err := p.db.WithContext(ctx).Create(&m).Error
	if err != nil {
		return blank, err
	}

	return toDTO(m), nil
}

func (p *postgresRepository) DeleteAll(ctx context.Context) error {
	err := p.db.WithContext(ctx).Delete(&todo{}).Error
	if err != nil {
		return err
	}

	return nil
}

func (p *postgresRepository) Delete(ctx context.Context, id int) error {
	err := p.db.WithContext(ctx).Delete(&todo{}, id).Error
	if err != nil {
		return err
	}

	return nil
}

func (p *postgresRepository) Update(ctx context.Context, id int, t Todo) (Todo, error) {
	var m todo

	err := p.db.WithContext(ctx).First(&m, id).Error
	if err != nil {
		return blank, err
	}

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

	err = p.db.WithContext(ctx).Model(&m).Updates(toModel(t)).Error
	if err != nil {
		return blank, err
	}

	return toDTO(m), nil
}

func (p *postgresRepository) Drop() error {
	err := p.db.Unscoped().Delete(&todo{}).Error
	if err != nil {
		return err
	}

	return nil
}

type todo struct {
	gorm.Model
	Title     string `gorm:"type:text"`
	Completed bool
	Order     int
}

func toModel(t Todo) todo {
	return todo{
		Title:     *t.Title,
		Completed: *t.Completed,
		Order:     *t.Order,
	}
}

func toDTO(t todo) Todo {
	id := int(t.ID)
	return Todo{
		ID:        &id,
		Title:     &t.Title,
		Completed: &t.Completed,
		Order:     &t.Order,
	}
}
