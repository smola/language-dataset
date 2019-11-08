extends Sprite
class_name MovingSprite
tool


const icon = preload("res://icon.png")
const good_icon = preload("res://good.png")
const bad_icon = preload("res://bad.png")

enum process_modes {
    PROCESS,
    PHYSICS_PROCESS,
}

enum movement_modes {
    MOVE_AND_COLLIDE,
    MOVE_AND_COLLIDE_TIMES_DELTA,
    MOVE_AND_COLLIDE_TIMES_DELTA_TWICE,
    MOVE_AND_SLIDE,
    MOVE_AND_SLIDE_TIMES_DELTA,
    MOVE_AND_SLIDE_TIMES_DELTA_TWICE,
}

enum speed_modes {
    CONSTANT_SPEED,
    ACCELERATE,
    ACCELERATE_TIMES_DELTA,
    ACCELERATE_TIMES_DELTA_TWICE,
}

export(process_modes) var process_mode := process_modes.PROCESS setget set_process_mode
export(movement_modes) var movement_mode := movement_modes.MOVE_AND_COLLIDE setget set_movement_mode
export(speed_modes) var speed_mode := speed_modes.CONSTANT_SPEED setget set_speed_mode
export(float, EXP, 0.01, 5000) var speed := 5.0 setget set_speed

export(float, 1, 1000) var max_distance = 352.0 setget set_max_distance # Pixels
export(float, EXP, 100, 5000) var max_velocity = 1000.0 setget set_max_velocity  # Resets velocity if moving faster than this

#var feedback_sprite = Sprite.new()
#var feedback_label = Label.new()
#var feedback_panel = Panel.new()

var _distance := 0.0 setget set_distance  # Abstracts away horizontal position
var _velocity := 0.0 setget set_velocity  # Amount to move each frame

# Declare member variables here. Examples:
# var a: int = 2
# var b: String = "text"

# Called when the node enters the scene tree for the first time.
func _ready() -> void:
    add_to_group("MovingSprite")
    texture = icon
    reset()


# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta: float) -> void:
    if not process_mode == process_modes.PROCESS:
        return
    move(delta)


# Called every physics frame. 'delta' is the elapsed time since the previous physics frame.
# Defaults to 60 FPS, can be changed in project settings under 'physics/common/physics_fps'
func _physics_process(delta: float) -> void:
    if not process_mode == process_modes.PHYSICS_PROCESS:
        return
    move(delta)


func move(delta: float) -> void:
    
    match speed_mode:
        speed_modes.CONSTANT_SPEED:
            set_velocity(speed)  # good!
        speed_modes.ACCELERATE:
            set_velocity(_velocity + speed)  # bad!
        speed_modes.ACCELERATE_TIMES_DELTA:
            set_velocity(_velocity + speed * delta)  # good!
        speed_modes.ACCELERATE_TIMES_DELTA_TWICE:
            set_velocity(_velocity + speed * delta * delta)  # bad!
        _:
            assert false
    
    match movement_mode:
        movement_modes.MOVE_AND_COLLIDE:
            set_distance(_distance + _velocity)  # bad!
        movement_modes.MOVE_AND_COLLIDE_TIMES_DELTA:
            set_distance(_distance + _velocity * delta)  # good!
        movement_modes.MOVE_AND_COLLIDE_TIMES_DELTA_TWICE:
            set_distance(_distance + _velocity * delta * delta)  # bad!
        movement_modes.MOVE_AND_SLIDE:
            set_distance(_distance + _velocity * delta)  # good!
        movement_modes.MOVE_AND_SLIDE_TIMES_DELTA:
            set_distance(_distance + _velocity * delta * delta)  # bad!
        movement_modes.MOVE_AND_SLIDE_TIMES_DELTA_TWICE:
            set_distance(_distance + _velocity * delta * delta * delta)  # very bad!!!
        _:
            assert false
    
    if Engine.is_editor_hint() and process_mode == process_modes.PROCESS:
        # The editor has v-sync enabled, so its framerate is capped and the effects
        # of improperly using delta are less noticeable. To combat this, the effects
        # of delta are multiplied when in the editor. This isn't really accurate,
        # but at least it gets the point across when something is wrong.
        var simulate_real_framerate := 19.0
        match speed_mode:
            speed_modes.ACCELERATE:
                set_velocity(_velocity + speed * simulate_real_framerate)
            speed_modes.ACCELERATE_TIMES_DELTA_TWICE:
                set_velocity(_velocity + speed * delta * delta / simulate_real_framerate)
        match movement_mode:
            movement_modes.MOVE_AND_COLLIDE:
                set_distance(_distance + _velocity * simulate_real_framerate)
            movement_modes.MOVE_AND_COLLIDE_TIMES_DELTA_TWICE, movement_modes.MOVE_AND_SLIDE_TIMES_DELTA:
                set_distance(_distance + _velocity * delta * delta / simulate_real_framerate)
            movement_modes.MOVE_AND_SLIDE_TIMES_DELTA_TWICE:
                set_distance(_distance + _velocity * delta * delta * delta / simulate_real_framerate / simulate_real_framerate)


func set_process_mode(new_process_mode: int) -> void:
    process_mode = new_process_mode
    reset()


func set_movement_mode(new_movement_mode: int) -> void:
    movement_mode = new_movement_mode
    reset()


func set_speed_mode(new_speed_mode: int) -> void:
    speed_mode = new_speed_mode
    reset()


func set_distance(new_distance: float) -> void:
    while new_distance >= max_distance:
        new_distance -= max_distance
    _distance = new_distance
    offset.x = (1 - abs(_distance / max_distance * 2 - 1)) * max_distance


func set_velocity(new_velocity: float) -> void:
    if speed_mode == speed_modes.CONSTANT_SPEED:
        new_velocity = min(new_velocity, max_velocity)
    else:
        while new_velocity >= max_velocity:
            new_velocity -= max_velocity
    _velocity = new_velocity


func set_speed(new_speed: float) -> void:
    speed = new_speed
    reset()


func set_max_distance(new_max_distance: float) -> void:
    max_distance = new_max_distance
    reset()


func set_max_velocity(new_max_velocity: float) -> void:
    max_velocity = new_max_velocity
    reset()


func reset() -> void:
    for child in get_children():
        remove_child(child)
    update_feedback_label()
    update_feedback_sprite()
    if not is_inside_tree():
        return
    get_tree().call_group("MovingSprite", "_reset")


func _reset() -> void:
    set_distance(0.0)
    set_velocity(0.0)


func update_feedback_sprite() -> void:
    var correct_movement_mode: bool = false
    var correct_speed_mode: bool = false

    var feedback_sprite = Sprite.new()

    feedback_sprite.offset.x = -96
    
    match movement_mode:
        movement_modes.MOVE_AND_COLLIDE_TIMES_DELTA, movement_modes.MOVE_AND_SLIDE:
            correct_movement_mode = true
    
    match speed_mode:
        speed_modes.CONSTANT_SPEED, speed_modes.ACCELERATE_TIMES_DELTA:
            correct_speed_mode = true
    
    if correct_movement_mode and correct_speed_mode:
        feedback_sprite.texture = good_icon
    else:
        feedback_sprite.texture = bad_icon
    
    add_child(feedback_sprite)


func update_feedback_label() -> void:
    var text := ""
    
    var feedback_label = Label.new()
    var feedback_panel = Panel.new()

    feedback_label.rect_position = Vector2(-120, -40)
    feedback_label.grow_vertical = Control.GROW_DIRECTION_BEGIN
    feedback_panel.anchor_right = 1.0
    feedback_panel.anchor_bottom = 1.0
    feedback_panel.show_behind_parent = true
    feedback_panel.margin_left = -8
    feedback_panel.margin_right = 8
    feedback_panel.margin_top = -8
    feedback_panel.margin_bottom = 8
    
    match process_mode:
        process_modes.PROCESS:
            text += "func _process(delta: float) -> void:\n"
        process_modes.PHYSICS_PROCESS:
            text += "func _physics_process(delta: float) -> void:\n"
    
    var speed_text := ""
    
    match speed_mode:
        speed_modes.CONSTANT_SPEED:
            speed_text = "Vector2(%.0f, 0)" % speed if speed >= 1 else "Vector2(%.2f, 0)" % speed
        speed_modes.ACCELERATE:
            text += "    velocity += acceleration\n"
            speed_text = "velocity"
        speed_modes.ACCELERATE_TIMES_DELTA:
            text += "    velocity += acceleration * delta\n"
            speed_text = "velocity"
        speed_modes.ACCELERATE_TIMES_DELTA_TWICE:
            text += "    velocity += acceleration * delta * delta\n"
            speed_text = "velocity"
    
    match movement_mode:
        movement_modes.MOVE_AND_COLLIDE:
            text += "    move_and_collide(%s)" % speed_text
        movement_modes.MOVE_AND_COLLIDE_TIMES_DELTA:
            text += "    move_and_collide(%s * delta)" % speed_text
        movement_modes.MOVE_AND_COLLIDE_TIMES_DELTA_TWICE:
            text += "    move_and_collide(%s * delta * delta)" % speed_text
        movement_modes.MOVE_AND_SLIDE:
            text += "    move_and_slide(%s)" % speed_text
        movement_modes.MOVE_AND_SLIDE_TIMES_DELTA:
            text += "    move_and_slide(%s * delta)" % speed_text
        movement_modes.MOVE_AND_SLIDE_TIMES_DELTA_TWICE:
            text += "    move_and_slide(%s * delta * delta)" % speed_text
    
    feedback_label.text = text
    
    add_child(feedback_label)
    feedback_label.add_child(feedback_panel)

