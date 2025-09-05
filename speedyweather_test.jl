using SpeedyWeather
spectral_grid = SpectralGrid()

model = ShallowWaterModel(spectral_grid)

# Most model components depend on SpectralGrid as a first argment
model.time_stepping

spectral_grid = SpectralGrid(trunc=63, nlayers=1)
my_time_stepping = Leapfrog(spectral_grid, Δt_at_T31=Minute(15))

# Create a model by passing on the components
model = ShallowWaterModel(spectral_grid; time_stepping = my_time_stepping)

# Can define new parameters of model using <: 

# A model is a collection of model components that beloing into one of 3 groups 

# Components (numerics, dynamics, physics) that have parameter and possibly contain some data (immutable and precomputed)
# + some functions associated with them. E.g. 
# Components that are just a collection of parameters that are conceptually linked
# Components for output purposes 

# Constructing different models with :
# spectral_grid = SpectralGrid(trunc = ..., ...)
# component1 = SomeComponent(spectral_grid, parameter1 = ..., ...)
# component2 = SomeComponent(spectral_grid, parameters = ..., ...)
# model = BarotropicModel(spectral_grid, component1, component2, ...)

simulation = initialize!(model)
# Initialize model at a specific time
simulation = initialize!(model, time = DateTime(2020,5,1))
simulation.prognostic_variables.clock.time

# Can change the output time step but can't change everything of the model once its been initialized
simulation.model.output.output_dt = Second(3600)

run!(simulation, output = true)