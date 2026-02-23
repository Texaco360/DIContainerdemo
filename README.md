# Dependency Injection Container Demo

A Free Pascal implementation of a dependency injection container with different service lifetimes (Singleton, Transient, Scoped).

## Project Structure

```
DIContainerdemo/
├── src/                          # Source code
│   ├── AppContainer.pas          # Main DI container implementation
│   ├── LoggerIntf.pas           # Logger interface
│   ├── Loggers.pas              # Basic logger implementations
│   ├── EnhancedLoggers.pas      # Advanced loggers with lifetime demos
│   ├── CalculationService.pas   # Example service implementation
│   ├── Demo.pas                 # Basic demo program
│   ├── TestLifetimeFeatures.pas # Lifetime testing program
│   ├── ContainerLifetimeTests.pas # Test framework
│   ├── LifetimeDemo.pas         # Advanced lifetime examples
│   ├── WebAppLifetimeDemo.pas   # Web application simulation
│   └── CalculationService_FakeLoggerTest.pas # Unit test example
├── build/                       # Compiled output (executables, .ppu, .o files)
├── docs/                        # Documentation
│   └── ContainerPatternGuide.md # Detailed pattern guide
├── .vscode/                     # VS Code configuration
└── README.md                    # This file
```

## Building the Project

### Using VS Code Tasks (Recommended)

Press `Ctrl+Shift+P` and type "Tasks: Run Task" then select:

- **Compile Demo (Enhanced Debug)** - Builds the main demo with full debug info
- **Compile TestLifetimeFeatures (Debug)** - Builds the test suite
- **Compile All Programs** - Builds all programs at once
- **Clean Build Files** - Cleans the build directory
- **Run Demo** - Compiles and runs the main demo
- **Run Tests** - Compiles and runs the test suite

### Using Command Line

```bash
# Compile main demo
fpc src/Demo.pas -FUbuild -FEbuild -g -gl

# Compile test suite  
fpc src/TestLifetimeFeatures.pas -FUbuild -FEbuild -g -gl

# Run programs
./build/Demo
./build/TestLifetimeFeatures

# Clean build files
rm -rf build/*
```

## Programs

- **Demo** - Basic dependency injection demonstration
- **TestLifetimeFeatures** - Comprehensive lifetime management tests

## Features

- **Singleton Lifetime** - One instance for the entire application
- **Transient Lifetime** - New instance every time it's requested  
- **Scoped Lifetime** - One instance per scope (e.g., per web request)
- **Factory Pattern** - Compatible with Free Pascal 3.2.2
- **Interface-based** - Clean separation of concerns
- **Thread-safe** - Ready for multi-threaded applications

## Requirements

- Free Pascal Compiler (FPC) 3.2.2 or later
- Linux/Windows/macOS

## Getting Started

1. Open the project in VS Code
2. Press `Ctrl+Shift+P` → "Tasks: Run Task" → "Run Demo"
3. Check the output to see dependency injection in action
4. Run "Run Tests" to see all lifetime behaviors tested

## Documentation

See [docs/ContainerPatternGuide.md](docs/ContainerPatternGuide.md) for detailed explanations of the dependency injection patterns implemented in this project.