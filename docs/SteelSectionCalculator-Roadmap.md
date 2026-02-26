# Steel Section Properties Calculator - Evolution Roadmap

A comprehensive roadmap for evolving from a simple DI container demo to a professional Steel Section Properties Calculator application, following SOLID principles and clean architecture.

## 🎯 Project Vision

Create a modular steel section calculator that can evolve from console application to GUI, REST API, and Python library while maintaining clean separation between business logic and presentation layers.

## 🏗️ Architecture Goals

- **Clean Architecture**: Business logic independent of frameworks
- **SOLID Principles**: Maintainable, extensible, testable code
- **Dependency Injection**: Loose coupling, easy testing
- **Multiple Interfaces**: Console, GUI, API, Library sharing same core

---

## Phase 1: Project Foundation & Architecture (Week 1-2)

### 1.1 Folder Structure
```
SteelSectionCalculator/
├── src/
│   ├── Core/                    # 🎯 Business Logic (Framework Independent)
│   │   ├── Interfaces/          # Contracts & abstractions
│   │   ├── Models/              # Domain entities
│   │   ├── Services/            # Business services
│   │   └── Calculations/        # Calculation engines
│   ├── Infrastructure/          # 🔧 External concerns
│   │   ├── Data/               # Data access, file I/O
│   │   ├── DI/                 # Dependency injection setup
│   │   └── Logging/            # Logging implementation
│   ├── Console/                 # 🖥️ Console application
│   │   ├── SteelCalculatorApp.pas   # Main program unit
│   │   ├── ConsoleInterface.pas     # Console UI logic
│   │   └── CommandParser.pas        # Command line parsing
│   ├── GUI/                     # 🖼️ Future GUI (Lazarus/fpGUI)
│   └── API/                     # 🌐 Future REST API
├── data/
│   ├── profiles/               # Steel profile databases
│   └── standards/              # Design standards (EC3, AISC, etc.)
├── tests/
│   ├── Core/                   # Unit tests for business logic
│   └── Integration/            # Integration tests
├── build/                      # Compiled outputs
├── docs/                       # Documentation
└── examples/                   # Usage examples
```

### 1.2 SOLID Principles Implementation
- **S**ingle Responsibility: Each class has one reason to change
- **O**pen/Closed: Extend via interfaces, not modification
- **L**iskov Substitution: Implementations are interchangeable
- **I**nterface Segregation: Small, focused interfaces
- **D**ependency Inversion: Depend on abstractions, not concretions

### 1.3 Deliverables
- [ ] Create complete folder structure
- [ ] Set up build system with VS Code tasks
- [ ] Initialize Git repository with .gitignore
- [ ] Create project README.md

---

## Phase 2: Core Domain Model (Week 3-4)

### 2.1 Define Core Interfaces

**File: `src/Core/Interfaces/SectionInterfaces.pas`**
```pascal
unit SectionInterfaces;

interface

type
  // Domain entities
  ISectionProfile = interface
    ['{GUID-HERE}']
    function GetName: string;
    function GetType: string;
    function GetDimensions: TDimensions;
    property Name: string read GetName;
  end;

  ISectionProperties = interface
    ['{GUID-HERE}']
    function GetArea: Double;
    function GetMomentOfInertiaY: Double;
    function GetMomentOfInertiaZ: Double;
    function GetSectionModulusY: Double;
    function GetSectionModulusZ: Double;
    function GetRadiusOfGyrationY: Double;
    function GetRadiusOfGyrationZ: Double;
  end;

  // Services
  ISectionCalculator = interface
    ['{GUID-HERE}']
    function Calculate(const Profile: ISectionProfile): ISectionProperties;
    function CalculateWithLoads(const Profile: ISectionProfile; 
      const Loads: TLoadCombination): IDesignResult;
  end;

  ISectionRepository = interface
    ['{GUID-HERE}']
    function FindByName(const Name: string): ISectionProfile;
    function GetAllProfiles: TArray<ISectionProfile>;
    function GetProfilesByType(const SectionType: string): TArray<ISectionProfile>;
    function SearchProfiles(const Criteria: ISearchCriteria): TArray<ISectionProfile>;
  end;

  IDesignStandard = interface
    ['{GUID-HERE}']
    function GetName: string;
    function GetCode: string;
    function ValidateSection(const Props: ISectionProperties; 
      const Load: TLoadCombination): IDesignResult;
    function GetSafetyFactors: TSafetyFactors;
  end;

  ILogger = interface
    ['{GUID-HERE}']
    procedure LogInfo(const Message: string);
    procedure LogWarning(const Message: string);
    procedure LogError(const Message: string);
    procedure LogDebug(const Message: string);
  end;
end;
```

### 2.2 Implement Core Models

**File: `src/Core/Models/SectionModels.pas`**
```pascal
unit SectionModels;

interface

uses
  SectionInterfaces;

type
  TSectionType = (stIBeam, stHBeam, stChannel, stAngle, stTube, stRectangular, stCircular);
  
  TDimensions = record
    Height: Double;        // h
    Width: Double;         // b
    WebThickness: Double;  // tw
    FlangeThickness: Double; // tf
    Radius: Double;        // r (for rounded corners)
  end;

  TLoadCombination = record
    AxialForce: Double;    // N
    BendingMomentY: Double; // My
    BendingMomentZ: Double; // Mz
    ShearForceY: Double;   // Vy
    ShearForceZ: Double;   // Vz
    TorsionalMoment: Double; // Mt
  end;

  TSafetyFactors = record
    MaterialFactor: Double;  // γM
    LoadFactor: Double;      // γF
    ModelFactor: Double;     // γRd
  end;

  // Base profile implementation
  TSectionProfile = class(TInterfacedObject, ISectionProfile)
  private
    FName: string;
    FSectionType: TSectionType;
    FDimensions: TDimensions;
    FMaterial: string;
  public
    constructor Create(const AName: string; AType: TSectionType; 
      const ADimensions: TDimensions; const AMaterial: string = 'S355');
    function GetName: string;
    function GetType: string;
    function GetDimensions: TDimensions;
    property Name: string read GetName;
  end;

  // Specific profile types
  TIPEProfile = class(TSectionProfile)
  public
    constructor Create(const AName: string; const ADimensions: TDimensions);
  end;

  THEAProfile = class(TSectionProfile)
  public
    constructor Create(const AName: string; const ADimensions: TDimensions);
  end;

  TUPNProfile = class(TSectionProfile)
  public
    constructor Create(const AName: string; const ADimensions: TDimensions);
  end;
```

### 2.3 Deliverables
- [ ] Complete interface definitions
- [ ] Implement basic profile models (IPE, HEA, UPN)
- [ ] Create calculation result models
- [ ] Define load combination structures
- [ ] Set up unit tests for models

---

## Phase 3: Business Logic Implementation (Week 5-6)

### 3.1 Calculation Services

**File: `src/Core/Services/SectionCalculationService.pas`**
```pascal
unit SectionCalculationService;

interface

uses
  SectionInterfaces, SectionModels;

type
  TSectionCalculationService = class(TInterfacedObject, ISectionCalculator)
  private
    FLogger: ILogger;
    function CalculateIBeamProperties(const Dimensions: TDimensions): ISectionProperties;
    function CalculateChannelProperties(const Dimensions: TDimensions): ISectionProperties;
    function CalculateAngleProperties(const Dimensions: TDimensions): ISectionProperties;
  public
    constructor Create(const ALogger: ILogger);
    function Calculate(const Profile: ISectionProfile): ISectionProperties;
    function CalculateWithLoads(const Profile: ISectionProfile; 
      const Loads: TLoadCombination): IDesignResult;
  end;

implementation

uses
  Math, SysUtils;

constructor TSectionCalculationService.Create(const ALogger: ILogger);
begin
  inherited Create;
  FLogger := ALogger;
end;

function TSectionCalculationService.Calculate(const Profile: ISectionProfile): ISectionProperties;
var
  Dims: TDimensions;
begin
  FLogger.LogInfo('Calculating properties for: ' + Profile.Name);
  Dims := Profile.GetDimensions;
  
  // Route to appropriate calculation method based on profile type
  case Profile.GetType of
    'IPE', 'HEA', 'HEB': Result := CalculateIBeamProperties(Dims);
    'UPN': Result := CalculateChannelProperties(Dims);
    'L': Result := CalculateAngleProperties(Dims);
  else
    raise Exception.Create('Unsupported profile type: ' + Profile.GetType);
  end;
end;

function TSectionCalculationService.CalculateIBeamProperties(const Dimensions: TDimensions): ISectionProperties;
var
  Area, Iy, Iz, Wy, Wz, iy, iz: Double;
begin
  with Dimensions do
  begin
    // Cross-sectional area
    Area := Height * WebThickness + 2 * (Width - WebThickness) * FlangeThickness;
    
    // Moment of inertia about y-axis (strong axis)
    Iy := (Width * Power(Height, 3)) / 12 - 
          ((Width - WebThickness) * Power(Height - 2 * FlangeThickness, 3)) / 12;
    
    // Moment of inertia about z-axis (weak axis)  
    Iz := (2 * FlangeThickness * Power(Width, 3)) / 12 + 
          ((Height - 2 * FlangeThickness) * Power(WebThickness, 3)) / 12;
    
    // Section modulus
    Wy := Iy / (Height / 2);
    Wz := Iz / (Width / 2);
    
    // Radius of gyration
    iy := Sqrt(Iy / Area);
    iz := Sqrt(Iz / Area);
  end;
  
  Result := TSectionProperties.Create(Area, Iy, Iz, Wy, Wz, iy, iz);
end;
```

### 3.2 Design Standards

**File: `src/Core/Services/Eurocode3Standard.pas`**
```pascal
unit Eurocode3Standard;

interface

uses
  SectionInterfaces, SectionModels;

type
  TEurocode3Standard = class(TInterfacedObject, IDesignStandard)
  private
    FSteelGrade: string;
    FYieldStrength: Double; // fy
    FUltimateStrength: Double; // fu
  public
    constructor Create(const ASteelGrade: string = 'S355');
    function GetName: string;
    function GetCode: string;
    function ValidateSection(const Props: ISectionProperties; 
      const Load: TLoadCombination): IDesignResult;
    function GetSafetyFactors: TSafetyFactors;
  end;
```

### 3.3 Deliverables
- [ ] Complete section property calculations for common profiles
- [ ] Implement Eurocode 3 design checks
- [ ] Add AISC design standard
- [ ] Create comprehensive unit tests
- [ ] Add calculation validation against hand calculations

---

## Phase 4: Infrastructure Layer (Week 7)

### 4.1 Data Access

**File: `src/Infrastructure/Data/ProfileRepository.pas`**
```pascal
unit ProfileRepository;

interface

uses
  SectionInterfaces, SectionModels, Classes;

type
  TProfileRepository = class(TInterfacedObject, ISectionRepository)
  private
    FDataPath: string;
    FCache: TList;
    procedure LoadProfilesFromFile(const FileName: string);
    procedure LoadIPEProfiles;
    procedure LoadHEAProfiles;
    procedure LoadUPNProfiles;
  public
    constructor Create(const ADataPath: string);
    destructor Destroy; override;
    function FindByName(const Name: string): ISectionProfile;
    function GetAllProfiles: TArray<ISectionProfile>;
    function GetProfilesByType(const SectionType: string): TArray<ISectionProfile>;
    function SearchProfiles(const Criteria: ISearchCriteria): TArray<ISectionProfile>;
  end;
```

### 4.2 DI Container Setup

**File: `src/Infrastructure/DI/DIContainerConfig.pas`**
```pascal
unit DIContainerConfig;

interface

uses
  AppContainer;

type
  TApplicationModule = class
  public
    class procedure ConfigureServices(const Container: TAppContainer);
    class procedure ConfigureLogging(const Container: TAppContainer);
    class procedure ConfigureRepositories(const Container: TAppContainer);
    class procedure ConfigureServices(const Container: TAppContainer);
    class procedure ConfigureStandards(const Container: TAppContainer);
  end;

implementation

uses
  ProfileRepository, SectionCalculationService, Eurocode3Standard, 
  ConsoleLogger, FileLogger;

class procedure TApplicationModule.ConfigureServices(const Container: TAppContainer);
begin
  ConfigureLogging(Container);
  ConfigureRepositories(Container);
  ConfigureBusinessServices(Container);
  ConfigureStandards(Container);
end;

class procedure TApplicationModule.ConfigureLogging(const Container: TAppContainer);
begin
  Container.RegisterSingleton('logger.console', TConsoleLoggerFactory.Create);
  Container.RegisterSingleton('logger.file', TFileLoggerFactory.Create('steel-calc.log'));
end;

class procedure TApplicationModule.ConfigureRepositories(const Container: TAppContainer);
begin
  Container.RegisterSingleton('profileRepository', 
    TProfileRepositoryFactory.Create('data/profiles'));
end;

class procedure TApplicationModule.ConfigureBusinessServices(const Container: TAppContainer);
begin
  Container.RegisterScoped('sectionCalculator',
    TSectionCalculationServiceFactory.Create);
end;

class procedure TApplicationModule.ConfigureStandards(const Container: TAppContainer);
begin
  Container.RegisterSingleton('standard.eurocode3',
    TEurocode3StandardFactory.Create('S355'));
  Container.RegisterSingleton('standard.aisc',
    TAISCStandardFactory.Create('A992'));
end;
```

### 4.3 Deliverables
- [ ] File-based profile repository with CSV/JSON support
- [ ] Complete DI container configuration
- [ ] Factory classes for all services
- [ ] Configuration management system
- [ ] Database connection setup (optional)

---

## Phase 5: Console Application (Week 8)

### 5.1 Main Program

**File: `src/Console/SteelCalculatorApp.pas`**
```pascal
program SteelCalculatorApp;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  // DI Container
  AppContainer, DIContainerConfig,
  // Console Interface
  ConsoleInterface, CommandParser;

var
  Container: TAppContainer;
  ConsoleApp: TConsoleApplication;
begin
  WriteLn('Steel Section Properties Calculator v1.0');
  WriteLn('=====================================');
  WriteLn;
  
  try
    // Initialize DI Container
    Container := TAppContainer.Create;
    TApplicationModule.ConfigureServices(Container);
    
    // Create and run console application
    ConsoleApp := TConsoleApplication.Create(Container);
    try
      ConsoleApp.Run;
    finally
      ConsoleApp.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Fatal Error: ', E.Message);
      WriteLn('Press Enter to exit...');
      ReadLn;
      ExitCode := 1;
    end;
  finally
    Container.Free;
  end;
end.
```

### 5.2 Console Interface

**File: `src/Console/ConsoleInterface.pas`**
```pascal
unit ConsoleInterface;

interface

uses
  SysUtils, AppContainer, SectionInterfaces;

type
  TConsoleApplication = class
  private
    FContainer: TAppContainer;
    FSectionCalculator: ISectionCalculator;
    FProfileRepository: ISectionRepository;
    FDesignStandard: IDesignStandard;
    FLogger: ILogger;
    
    procedure ShowMainMenu;
    procedure ListProfiles;
    procedure CalculateSection;
    procedure DesignCheck;
    procedure ShowHelp;
    function GetUserChoice: Integer;
    function SelectProfile: ISectionProfile;
  public
    constructor Create(const AContainer: TAppContainer);
    procedure Run;
  end;

implementation

procedure TConsoleApplication.ShowMainMenu;
begin
  WriteLn('=== MAIN MENU ===');
  WriteLn('1. List available profiles');
  WriteLn('2. Calculate section properties');
  WriteLn('3. Perform design check');
  WriteLn('4. Search profiles');
  WriteLn('5. Help');
  WriteLn('0. Exit');
  WriteLn;
  Write('Enter your choice (0-5): ');
end;

procedure TConsoleApplication.Run;
var
  Choice: Integer;
  Continue: Boolean;
begin
  Continue := True;
  
  while Continue do
  begin
    ShowMainMenu;
    Choice := GetUserChoice;
    WriteLn;
    
    case Choice of
      1: ListProfiles;
      2: CalculateSection;
      3: DesignCheck;
      4: SearchProfiles;
      5: ShowHelp;
      0: Continue := False;
    else
      WriteLn('Invalid choice. Please try again.');
    end;
    
    if Continue then
    begin
      WriteLn;
      WriteLn('Press Enter to continue...');
      ReadLn;
    end;
  end;
  
  WriteLn('Thank you for using Steel Section Calculator!');
end;
```

### 5.3 Deliverables
- [ ] Complete console application with menu system
- [ ] Command-line argument parsing
- [ ] Interactive profile selection
- [ ] Results formatting and display
- [ ] Error handling and user-friendly messages
- [ ] Help system and documentation

---

## Phase 6: Testing Strategy (Ongoing)

### 6.1 Unit Tests Structure
```
tests/
├── Core/
│   ├── TestSectionCalculations.pas    # Business logic tests
│   ├── TestProfileModels.pas          # Domain model tests
│   ├── TestDesignStandards.pas        # Standards compliance tests
│   └── TestInterfaces.pas             # Interface contract tests
├── Infrastructure/
│   ├── TestProfileRepository.pas      # Data access tests
│   ├── TestDIContainer.pas           # DI container tests
│   └── TestLogging.pas               # Logging tests
├── Console/
│   └── TestConsoleInterface.pas      # Console UI tests
└── TestRunner.pas                    # Main test runner
```

### 6.2 Test Categories
- **Unit Tests**: Individual class testing
- **Integration Tests**: Component interaction testing  
- **Calculation Tests**: Mathematical accuracy validation
- **Performance Tests**: Large dataset handling
- **Regression Tests**: Prevent breaking changes

### 6.3 Deliverables
- [ ] Comprehensive unit test suite (>80% coverage)
- [ ] Integration tests for all major workflows
- [ ] Performance benchmarks
- [ ] Continuous integration setup
- [ ] Test data fixtures and mocks

---

## Phase 7: Future Extensions

### 7.1 GUI Application (Lazarus/fpGUI)

**Structure:**
```
src/GUI/
├── SteelCalculatorGUI.pas         # Main GUI program
├── Forms/
│   ├── MainForm.pas               # Main application window
│   ├── ProfileBrowserForm.pas     # Profile selection dialog
│   ├── CalculationForm.pas        # Calculation input/results
│   └── SettingsForm.pas          # Application settings
├── Controllers/
│   ├── MainController.pas         # Main form controller
│   └── CalculationController.pas  # Calculation workflow
└── Components/
    └── SectionViewer.pas          # Custom section drawing component
```

**Features:**
- Interactive profile browser with search/filter
- Visual section property display with diagrams
- Batch calculation capabilities
- Results export (PDF, Excel)
- Print functionality

### 7.2 REST API (fpWeb/Brook Framework)

**Structure:**
```
src/API/
├── SteelCalculatorAPI.pas         # Main API server
├── Controllers/
│   ├── ProfileController.pas      # Profile endpoints
│   ├── CalculationController.pas  # Calculation endpoints
│   └── HealthController.pas       # Health check endpoints
├── Middleware/
│   ├── AuthenticationMiddleware.pas
│   └── LoggingMiddleware.pas
└── Models/
    └── APIModels.pas              # JSON request/response models
```

**Endpoints:**
- `GET /api/profiles` - List all profiles
- `GET /api/profiles/{name}` - Get specific profile
- `POST /api/calculate` - Calculate section properties
- `POST /api/design-check` - Perform design validation

### 7.3 Python Integration

**Approaches:**

1. **Shared Library (.so/.dll):**
```pascal
// Create C-compatible wrapper functions
library SteelSectionLib;

{$mode objfpc}

uses
  SectionInterfaces, SectionCalculationService;

function CalculateIPE(height, width, webThickness, flangeThickness: Double;
  out area, iy, iz: Double): Integer; cdecl;
// Implementation
exports CalculateIPE;

end.
```

2. **Python ctypes integration:**
```python
import ctypes

# Load the Pascal library
steel_lib = ctypes.CDLL('./libSteelSection.so')

# Define function signature
steel_lib.CalculateIPE.argtypes = [ctypes.c_double, ctypes.c_double, 
                                   ctypes.c_double, ctypes.c_double,
                                   ctypes.POINTER(ctypes.c_double),
                                   ctypes.POINTER(ctypes.c_double),
                                   ctypes.POINTER(ctypes.c_double)]

# Use in Python
area, iy, iz = ctypes.c_double(), ctypes.c_double(), ctypes.c_double()
result = steel_lib.CalculateIPE(400.0, 180.0, 8.6, 13.5, 
                               ctypes.byref(area), ctypes.byref(iy), ctypes.byref(iz))
```

3. **REST API Client:**
```python
import requests

class SteelCalculatorClient:
    def __init__(self, base_url='http://localhost:8080'):
        self.base_url = base_url
    
    def calculate_section(self, profile_name):
        response = requests.post(f'{self.base_url}/api/calculate',
                               json={'profile': profile_name})
        return response.json()
```

---

## 📋 Implementation Checklist

### ✅ Phase 1: Foundation (Week 1-2)
- [ ] Create complete folder structure
- [ ] Set up build system with VS Code tasks  
- [ ] Initialize Git repository with proper .gitignore
- [ ] Create comprehensive project documentation
- [ ] Set up development environment

### ✅ Phase 2: Core Domain (Week 3-4)
- [ ] Define all core interfaces
- [ ] Implement profile models (IPE, HEA, UPN, L-profiles)
- [ ] Create calculation result structures
- [ ] Define load combination types
- [ ] Set up basic unit tests

### ✅ Phase 3: Business Logic (Week 5-6)
- [ ] Implement section property calculations
- [ ] Create Eurocode 3 design standard
- [ ] Add AISC design standard
- [ ] Implement load combination checks
- [ ] Create calculation validation tests

### ✅ Phase 4: Infrastructure (Week 7)
- [ ] File-based profile repository
- [ ] Complete DI container configuration
- [ ] Factory pattern implementations
- [ ] Logging infrastructure
- [ ] Configuration management

### ✅ Phase 5: Console App (Week 8)
- [ ] Main program with DI setup
- [ ] Interactive console interface
- [ ] Menu system and navigation
- [ ] Error handling and validation
- [ ] Help system and documentation

### ✅ Phase 6: Testing (Ongoing)
- [ ] Unit tests for all business logic
- [ ] Integration tests for workflows
- [ ] Performance testing
- [ ] Test automation setup
- [ ] Code coverage reporting

### 🚀 Future Phases
- [ ] **GUI Application**: Lazarus-based desktop interface
- [ ] **REST API**: Web service for remote access
- [ ] **Python Library**: ctypes or REST client integration
- [ ] **Web Frontend**: React/Angular SPA consuming API
- [ ] **Mobile App**: Cross-platform calculator

---

## 🛠️ Technical Decisions

### Language & Frameworks
- **Core**: Free Pascal/Object Pascal
- **GUI**: Lazarus (cross-platform native)
- **API**: fpWeb or Brook Framework
- **Database**: SQLite for profiles, PostgreSQL for enterprise
- **Testing**: DUnit2 or FPCUnit

### Design Patterns
- **Dependency Injection**: Constructor injection with interface contracts
- **Repository Pattern**: Data access abstraction
- **Factory Pattern**: Object creation management
- **Strategy Pattern**: Multiple design standards
- **Observer Pattern**: Progress notifications (future GUI)

### Quality Assurance
- **Unit Testing**: >80% code coverage target
- **Static Analysis**: PasDoc for documentation
- **Code Reviews**: Git-based workflow
- **Continuous Integration**: GitHub Actions or similar
- **Performance Monitoring**: Benchmarking critical calculations

---

## 📚 Learning Resources

### Steel Design Standards
- **Eurocode 3**: EN 1993-1-1 Design of steel structures
- **AISC**: Steel Construction Manual (US standard)
- **CSA S16**: Canadian steel design standard
- **AS 4100**: Australian steel structures standard

### Technical References
- **Structural Analysis**: Hibbeler, Kassimali
- **Steel Design**: McCormac, Segui
- **Programming**: Free Pascal documentation, Object Pascal tutorials

This roadmap provides a structured approach to building a professional steel section calculator while maintaining clean architecture and following SOLID principles throughout the development process.