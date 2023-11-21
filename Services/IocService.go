package Services

import (
	"errors"
	"fmt"
	"reflect"
	"sync"
)

const (
	DefaultBuildFuncName = "Build"
)

type IocSingleton struct {
	Value       interface{}
	FinishBuild bool
	BuildOnce   sync.Once
}

func (this *IocSingleton) Build(iocService *IocService, tmpScopedMap map[reflect.Type]interface{}) error {
	var err error = nil
	if this.FinishBuild {
		return nil
	}
	this.BuildOnce.Do(func() {
		err = iocService.build(this.Value, tmpScopedMap)
		this.FinishBuild = true
	})
	return err
}

type IocObject struct {
	Type       reflect.Type
	OriginType reflect.Type
}

func newObjectByReflectType(reflectType reflect.Type) interface{} {
	if reflectType.Kind() == reflect.Ptr {
		return reflect.New(reflectType.Elem()).Interface()
	} else if reflectType.Kind() == reflect.Struct {
		return reflect.New(reflectType).Interface()
	} else {
		panic("invalid type")
	}
}

func (this *IocObject) New() interface{} {
	return newObjectByReflectType(this.OriginType)
}

type TmpScopedGenerator struct {
	scopedMap map[reflect.Type]interface{}
}

func NewTmpScopedGenerator() *TmpScopedGenerator {
	return &TmpScopedGenerator{
		scopedMap: map[reflect.Type]interface{}{},
	}
}

func (this *TmpScopedGenerator) AddScoped(scoped interface{}) {
	this.scopedMap[reflect.TypeOf(scoped)] = scoped
}

func AddTmpScoped(generator *TmpScopedGenerator, scoped interface{}) {
	generator.AddScoped(scoped)
}

func (this *TmpScopedGenerator) AddScopedInterface(scoped interface{}, interfaceNilPtr interface{}) {
	serviceType := reflect.TypeOf(interfaceNilPtr)
	if serviceType.Kind() == reflect.Ptr && serviceType.Elem() != nil && serviceType.Elem().Kind() == reflect.Interface {
		serviceType = serviceType.Elem()
	}
	this.scopedMap[serviceType] = scoped
}

func AddTmpScopedInterface[Interface any](generator *TmpScopedGenerator, scoped interface{}) {
	var defaultValue *Interface
	generator.AddScopedInterface(scoped, defaultValue)
}

type IocService struct {
	Singletons      map[reflect.Type]*IocSingleton
	SingletonValues map[interface{}]*IocSingleton
	Scopes          map[reflect.Type]*IocObject
	Transients      map[reflect.Type]*IocObject
	BuildFuncName   string
}

func createIocService() *IocService {
	return &IocService{
		BuildFuncName:   DefaultBuildFuncName,
		Singletons:      map[reflect.Type]*IocSingleton{},
		SingletonValues: map[interface{}]*IocSingleton{},
		Scopes:          map[reflect.Type]*IocObject{},
		Transients:      map[reflect.Type]*IocObject{},
	}
}

func SetBuildFuncName(buildFuncName string) {
	iocService.BuildFuncName = buildFuncName
}

func getReflectTypeName(objectType reflect.Type) string {
	if objectType == nil {
		return "nil"
	}
	if objectType.Kind() == reflect.Ptr {
		return "*" + getReflectTypeName(objectType.Elem())
	}
	return objectType.Name()
}

func (this *IocService) RegisterSingleton(singletonValue interface{}, needBuild bool) error {
	singleton, ok := this.SingletonValues[singletonValue]
	if !ok {
		singleton = &IocSingleton{
			Value:       singletonValue,
			FinishBuild: !needBuild,
		}
		this.SingletonValues[singletonValue] = singleton
	}
	this.Singletons[reflect.TypeOf(singletonValue)] = singleton
	return nil
}

func (this *IocService) RegisterSingletonInterface(singletonValue interface{}, interfacePtr interface{}, needBuild bool) error {
	singletonType := reflect.TypeOf(interfacePtr)
	if singletonType.Kind() == reflect.Ptr && singletonType.Elem() != nil && singletonType.Elem().Kind() == reflect.Interface {
		singletonType = singletonType.Elem()
	}
	singleton, ok := this.SingletonValues[singletonValue]
	if !ok {
		singleton = &IocSingleton{
			Value:       singletonValue,
			FinishBuild: !needBuild,
		}
		this.SingletonValues[singletonValue] = singleton
	}
	this.Singletons[singletonType] = singleton
	return nil
}

func RegisterSingletonT[T any]() error {
	var defaultValue T
	singletonValue := newObjectByReflectType(reflect.TypeOf(defaultValue))
	return iocService.RegisterSingleton(singletonValue, true)
}

func RegisterSingleton(singletonValue interface{}) error {
	return iocService.RegisterSingleton(singletonValue, false)
}

func RegisterSingletonInterface[Interface any](singletonValue interface{}, needBuild bool) error {
	var defaultInterface *Interface
	return iocService.RegisterSingletonInterface(singletonValue, defaultInterface, needBuild)
}

func RegisterSingletonInterfaceT[Interface any, T any]() error {
	var defaultValue T
	singletonValue := newObjectByReflectType(reflect.TypeOf(defaultValue))
	var defaultInterface *Interface
	return iocService.RegisterSingletonInterface(singletonValue, defaultInterface, true)
}

func (this *IocService) RegisterScoped(objectDemo interface{}) error {
	objectType := reflect.TypeOf(objectDemo)
	this.Scopes[objectType] = &IocObject{
		Type:       objectType,
		OriginType: objectType,
	}
	return nil
}

func RegisterScoped[T any]() error {
	var defaultValue T
	scopedValue := newObjectByReflectType(reflect.TypeOf(defaultValue))
	return iocService.RegisterScoped(scopedValue)
}

func (this *IocService) RegisterScopedInterface(objectDemo interface{}, interfaceNilPtr interface{}) error {
	originType := reflect.TypeOf(objectDemo)
	serviceType := reflect.TypeOf(interfaceNilPtr)
	if serviceType.Kind() == reflect.Ptr && serviceType.Elem().Kind() == reflect.Interface {
		serviceType = serviceType.Elem()
	}
	this.Scopes[serviceType] = &IocObject{
		Type:       serviceType,
		OriginType: originType,
	}
	return nil
}

func RegisterScopedInterface[Interface any, T any]() error {
	var defaultValue T
	singletonValue := newObjectByReflectType(reflect.TypeOf(defaultValue))
	var defaultInterface *Interface
	return iocService.RegisterScopedInterface(singletonValue, defaultInterface)
}

func (this *IocService) RegisterTransient(objectDemo interface{}) error {
	objectType := reflect.TypeOf(objectDemo)
	this.Transients[objectType] = &IocObject{
		Type:       objectType,
		OriginType: objectType,
	}
	return nil
}

func RegisterTransient[T any]() error {
	var defaultValue T
	scopedValue := newObjectByReflectType(reflect.TypeOf(defaultValue))
	return iocService.RegisterTransient(scopedValue)
}

func (this *IocService) RegisterTransientInterface(objectDemo interface{}, interfaceNilPtr interface{}) error {
	originType := reflect.TypeOf(objectDemo)
	serviceType := reflect.TypeOf(interfaceNilPtr)
	if serviceType.Kind() == reflect.Ptr && serviceType.Elem().Kind() == reflect.Interface {
		serviceType = serviceType.Elem()
	}
	this.Transients[serviceType] = &IocObject{
		Type:       serviceType,
		OriginType: originType,
	}
	return nil
}

func RegisterTransientInterface[Interface any, T any]() error {
	var defaultValue T
	singletonValue := newObjectByReflectType(reflect.TypeOf(defaultValue))
	var defaultInterface *Interface
	return iocService.RegisterTransientInterface(singletonValue, defaultInterface)
}

func (this *IocService) getSingleton(singletonType reflect.Type, tmpScopedMap map[reflect.Type]interface{}) (interface{}, error) {
	singleton, ok := this.Singletons[singletonType]
	if !ok {
		return nil, errors.New(fmt.Sprintf("without singleton:%s", getReflectTypeName(singletonType)))
	}
	singletonValue := this.SingletonValues[singleton.Value]
	err := singletonValue.Build(this, tmpScopedMap)
	if err != nil {
		return nil, err
	}
	return singleton.Value, nil
}

func (this *IocService) getScoped(scopedType reflect.Type, tmpScopedMap map[reflect.Type]interface{}) (interface{}, error) {
	scopedValue, find := tmpScopedMap[scopedType]
	if find {
		return scopedValue, nil
	}
	scoped, ok := this.Scopes[scopedType]
	if !ok {
		return nil, errors.New("without scoped:" + getReflectTypeName(scopedType))
	}
	scopedValue = scoped.New()
	tmpScopedMap[scopedType] = scopedValue
	err := this.build(scopedValue, tmpScopedMap)
	if err != nil {
		return nil, err
	}
	return scopedValue, nil
}

func (this *IocService) getTransient(transientType reflect.Type, tmpScopedMap map[reflect.Type]interface{}) (interface{}, error) {
	transient, ok := this.Transients[transientType]
	if !ok {
		return nil, errors.New("without transient:" + getReflectTypeName(transientType))
	}
	transientValue := transient.New()
	return transientValue, this.build(transientValue, tmpScopedMap)
}

func (this *IocService) NewObject(objectType reflect.Type) (interface{}, error) {
	if objectType.Kind() == reflect.Ptr {
		argObject := reflect.New(objectType.Elem()).Interface()
		err := this.Build(argObject)
		if err != nil {
			return nil, err
		}
		return argObject, nil
	}
	if objectType.Kind() == reflect.Struct {
		argObject := reflect.New(objectType).Interface()
		err := this.Build(argObject)
		argObject = reflect.ValueOf(argObject).Elem().Interface()
		if err != nil {
			return nil, err
		}
		return argObject, nil
	}
	return nil, errors.New("need Ptr or struct")
}

func GetOrNewT[T any]() (T, error) {
	var defaultValue T
	objectType := reflect.TypeOf(defaultValue)
	//try get or new from register data
	objectValue, err := iocService.getRegisterValue(objectType, map[reflect.Type]interface{}{})
	if err == nil {
		return objectValue.(T), nil
	}
	//new object
	objectValue, err = iocService.NewObject(reflect.TypeOf(defaultValue))
	if err != nil {
		return defaultValue, err
	}
	return objectValue.(T), nil
}

func (this *IocService) getRegisterValue(objectType reflect.Type, tmpScopedMap map[reflect.Type]interface{}) (interface{}, error) {
	//first ,try register by singleton
	argValue, err := this.getSingleton(objectType, tmpScopedMap)
	if err == nil {
		return argValue, nil
	}
	//second,try register by scoped
	argValue, err = this.getScoped(objectType, tmpScopedMap)
	if err == nil {
		return argValue, nil
	}
	//last, try register by transient
	argValue, err = this.getTransient(objectType, tmpScopedMap)
	if err == nil {
		return argValue, nil
	}
	return nil, errors.New("without register type:" + getReflectTypeName(objectType))
}

func (this *IocService) build(object interface{}, tmpScopedMap map[reflect.Type]interface{}) error {
	serviceValue := reflect.ValueOf(object)
	buildMethod := serviceValue.MethodByName(this.BuildFuncName)
	if !(buildMethod.Kind() == 0) {
		argCount := buildMethod.Type().NumIn()
		var in []reflect.Value
		for i := 0; i < argCount; i += 1 {
			argType := buildMethod.Type().In(i)
			argValue, err := this.getRegisterValue(argType, tmpScopedMap)
			if err != nil {
				return err
			}
			in = append(in, reflect.ValueOf(argValue))
		}
		out := buildMethod.Call(in)
		if len(out) == 1 && out[0].Type() == reflect.TypeOf((*error)(nil)).Elem() {
			errInterface := out[0].Interface()
			if errInterface != nil {
				initErr := out[0].Interface().(error)
				return errors.New(fmt.Sprintf("build object type:%s err:%s", getReflectTypeName(serviceValue.Type()), initErr.Error()))
			}
		}
	}
	return nil
}

func (this *IocService) Build(object interface{}) error {
	return this.build(object, map[reflect.Type]interface{}{})
}

func Build(object interface{}) error {
	return iocService.Build(object)
}

func (this *IocService) BuildWithoutTmpScoped(object interface{}, creator *TmpScopedGenerator) error {
	return this.build(object, creator.scopedMap)
}

func BuildWithoutTmpScoped(object interface{}, creator *TmpScopedGenerator) error {
	return iocService.build(object, creator.scopedMap)
}

var iocService *IocService

func init() {
	iocService = createIocService()
}
