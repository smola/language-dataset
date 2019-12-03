
/*
 * class Main
 * Created by Eqela Studio 2.0beta6
 */

class Main : SceneApplication
{
	public FrameController create_main_scene() {
		return(new MainMenuScene());
	}
}


public class MainMenuScene : SEScene
{
	SESprite background;
	SESprite title;
	SESprite start_btn;
	int scr_width;
	int scr_height;
	int btn_width;
	int btn_height;
	int btn_x;
	int btn_y;
	String space_str = "space";
	
	public void initialize(SEResourceCache src) {
		base.initialize(src);
		
		scr_width = get_scene_width();
		scr_height = get_scene_height();

		btn_width = scr_width / 10;
		btn_height = scr_height / 10;

		src.prepare_image("splash-bg", "space_bg", scr_width, scr_height);
		src.prepare_image("title", "title", scr_width / 2);

		background = add_sprite_for_image(SEImage.for_resource("splash-bg"));
		title = add_sprite_for_image(SEImage.for_resource("title"));
	}

	public void cleanup() {
		base.cleanup();
		background = SESprite.remove(background);
	}

	public void on_key_press(String name, String str) {
		base.on_key_press(name, str);
		if(space_str.equals(name)) {
			switch_scene(new GameScene());
		}
	}
}


public class GameScene : SEScene, SEPeriodicTimerHandler
{
	SESprite background;
	SESprite score_text;
	int scr_width;
	int scr_height;
	RocketEntity rocket;
	public int score=0;

	public void initialize(SEResourceCache src) {
		base.initialize(src);
		scr_width = get_scene_width();
		scr_height = get_scene_height();
		src.prepare_image("space-bg", "purple_space", scr_width, scr_height);

		add_entity(new BackgroundEntity());
		add_entity(BackgroundEntity.create(0-get_scene_height()));

		add_entity(SEPeriodicTimer.for_handler(this, 400000));
		rocket = new RocketEntity();
		add_entity(rocket);

		var score_h = scr_height / 10;
		if(score_h < 25) {
			score_h = 25;
		}
		src.prepare_font("arialwhite", "arial bold color=white", score_h);
		score_text = add_sprite_for_text("%d".printf().add(score).to_string(), "arialwhite");
		score_text.move(0, 0);
	}

	public void update(TimeVal now, double delta) {
		base.update(now, delta);
		var entities = get_entities();
		foreach(var b in entities) {
			if(b is RocketBeamEntity) {
				foreach(var e in iterate_entities()) {
					if(e is EnemyEntity) {
						if(check_collision(b as RocketBeamEntity, e as EnemyEntity)) {
							break;
						}
					}
				}
			}
		}
	}

	private bool check_collision(RocketBeamEntity b, EnemyEntity e) {
		var ey = e.get_ypos();
		var ex = e.get_xpos();
		var eh = e.height;
		var ew = e.width;
		var by = b.get_ypos();
		var bx = b.get_xpos();
		var bh = b.height;
		var bw = b.width;
		if(e.alive && 
			by<=ey+eh && by+bh>=ey &&
			bx+bw>=ex && bx<=ex+ew) {
			e.explode();
			b.explode();
			update_score();
			return(true);	
		}
		return(false);
	}

	private void update_score() {
		score = score + 10;
		score_text.set_text("%d".printf().add(score).to_string());
	}

	public void cleanup() {
		base.cleanup();
		score_text = SESprite.remove(score_text);
	}

	public bool on_timer(TimeVal now) {
		var c = Math.random(0, 5);
		int i, startx;
		startx = Math.random(50, get_scene_width()-50);
		add_entity(EnemyEntity.create(startx));
		return(true);
	}

	public RocketEntity get_rocket() {
		return(rocket);
	}

	public void end_game() {
		switch_scene(new GameScene());
	}
}


class RocketEntity : SEEntity
{
	SESprite rocket;
	int scr_width;
	int scr_height;
	int rocket_w;
	int rocket_h;
	double move_delta;
	TimeVal last_beam_time;
	int beam_rest=300000;
	GameScene gcn;
	public bool alive = true;
	TimeVal tod;

	public void initialize(SEResourceCache src) {
		base.initialize(src);

		gcn = get_scene() as GameScene;

		scr_width = get_scene_width();
		scr_height = get_scene_height();

		rocket_w = scr_width / 20;
		rocket_h = rocket_w * 12 / 11;
		
		src.prepare_image("rocket", "rocket", rocket_w, rocket_h);
		src.prepare_image("explosion0", "explosion0", rocket_w);

		rocket = add_sprite_for_image(SEImage.for_resource("rocket"));
		rocket.move((scr_width / 2) - (rocket_w / 2), (scr_height / 2) - (rocket_h / 2));
	}

	public void cleanup() {
		rocket = SESprite.remove(rocket);
	}

	public void tick(TimeVal now, double delta) {
		if(alive) {
			move_delta = 500 * delta;
			var rx = rocket.get_x();
			var ry = rocket.get_y();
			var rh = rocket.get_height();
			var rw = rocket.get_width();
			if(is_key_pressed("down") && ry+rh<gcn.get_scene_height()) {
				rocket.move(rx, ry+move_delta);
			}
			if(is_key_pressed("up") && ry>0) {
				rocket.move(rx, ry-move_delta);
			}
			if(is_key_pressed("left") && rx>0) {
				rocket.move(rx-move_delta, ry);
			}
			if(is_key_pressed("right") && rx+rw<gcn.get_scene_width()) {
				rocket.move(rx+move_delta, ry);
			}
			if(is_key_pressed("space")) {
				if(TimeVal.diff(last_beam_time, now)>beam_rest || last_beam_time==null){
					add_entity(RocketBeamEntity.create(rx+(rocket_w/2), ry));
					last_beam_time = now;
					beam_rest=300000;
				}
			}
			else {
				beam_rest=50000;
			}
		}
		else {
			rocket.set_alpha(rocket.get_alpha()-delta);
			if(tod==null) {
				tod = now;
			}
			else if (TimeVal.diff(tod, now)>2000000) {
				gcn.end_game();
			}
		}
	}

	public void explode() {
		rocket.set_image(SEImage.for_resource("explosion0"));
		alive = false;
	}

	public double get_ypos() {
		return(rocket.get_y());
	}

	public double get_xpos() {
		return(rocket.get_x());
	}

	public double get_width() {
		return(rocket.get_width());
	}

	public double get_height() {
		return(rocket.get_height());
	}
}


class EnemyEntity: SESpriteEntity
{
	double xspeed = 0.0;
	double yspeed = 300.0;
	int startx;
	GameScene gcn;
	RocketEntity rocket;
	bool shot = false;
	bool turned = false;
	public bool alive = true;
	public double width;
	public double height;

	public static EnemyEntity create(int start) {
		var e = new EnemyEntity();
		e.startx = start;
		return(e);
	}

	public void initialize(SEResourceCache rsc) {
		base.initialize(rsc);

		gcn = get_scene() as GameScene;
		rocket = gcn.get_rocket();
		
		rsc.prepare_image("enemy1", "enemy1", 0.05 * get_scene_width());
		rsc.prepare_image("explosion1", "explosion1", 0.05*get_scene_width());
		set_image(SEImage.for_resource("enemy1"));
		move(startx, -10);

		width = get_width();
		height = get_height();
	}

	public void tick(TimeVal now, double delta) {
		base.tick(now, delta);
		move(get_x() + xspeed * delta, get_y() + yspeed * delta);
		if(alive) {
			decide_to_shoot();
			decide_to_turn();
			detect_collision();
		}
		else {
			set_alpha(get_alpha()-delta);	
		}
		check_screen_visibility();
	}

	private void detect_collision() {
		var y = get_y();
		var x = get_x();
		var ry = rocket.get_ypos();
		var rx = rocket.get_xpos();
		var rw = rocket.get_width();
		if(rocket.alive && 
			y+get_height()>=ry && y<=ry+rocket.get_height() &&
			x>rx-get_width() && x<rx+rw) {
				explode();
				rocket.explode();
		}
	}

	private void decide_to_shoot() {
		var y = get_y();
		if((!shot) && (y<(gcn.get_scene_height()/3))) {
			var s = Math.random(1,100);
			if(s==1) {
				add_entity(EnemyBeamEntity.create(get_x()+(get_width()/2), y+get_height()));
				shot = true;	
			}
		}
	}

	private void decide_to_turn() {
		if(!turned) {
			var s = Math.random(1, 120);
			if(s==1) {
				if(shot) {
					xspeed = yspeed/2;
				}
				else {
					xspeed = -1*yspeed/2;
				}
				turned = true;
			}
		}
	}

	public void explode() {
		set_image(SEImage.for_resource("explosion1"));
		shot = true;
		turned = true;
		alive = false;
	}

	private void check_screen_visibility() {
		if (get_y()>gcn.get_scene_height()) {
			remove_entity();
		}
	}

	public double get_ypos() {
		return(get_y());
	}

	public double get_xpos() {
		return(get_x());
	}
}


class RocketBeamEntity: SESpriteEntity
{
	int xpos;
	int ypos;
	double yspeed = 1200.00;
	public double height;
	public double width;
	
	public static RocketBeamEntity create(int startx, int starty) {
		var b = new RocketBeamEntity();
		b.xpos = startx;
		b.ypos = starty;
		return(b);
	}
	
	public void initialize(SEResourceCache rsc) {
		base.initialize(rsc);
		
		rsc.prepare_image("beam1", "beam1", 0.02*get_scene_width());
		set_image(SEImage.for_resource("beam1"));
		move(xpos-(get_width()/2), ypos);

		width = get_width();
		height = get_height();
	}

	public void tick(TimeVal now, double delta) {
		base.tick(now, delta);
		move(get_x(), get_y() - yspeed * delta);
		check_screen_visibility();
	}

	public void explode() {
		remove_entity();
	}

	private void check_screen_visibility() {
		if (get_y()<0) {
			remove_entity();
		}
	}

	public double get_ypos() {
		return(get_y());
	}

	public double get_xpos() {
		return(get_x());
	}
}


class EnemyBeamEntity : SESpriteEntity
{
	int xpos;
	int ypos;
	double yspeed = 800.00;
	GameScene gcn;
	RocketEntity rocket;
	
	public static EnemyBeamEntity create(int startx, int starty) {
		var b = new EnemyBeamEntity();
		b.xpos = startx;
		b.ypos = starty;
		return(b);
	}
	
	public void initialize(SEResourceCache rsc) {
		base.initialize(rsc);

		gcn = get_scene() as GameScene;
		rocket = gcn.get_rocket();
		
		rsc.prepare_image("beam2", "beam2", 0.02*get_scene_width());
		set_image(SEImage.for_resource("beam2"));
		move(xpos-(get_width()/2), ypos-get_height());	
	}

	public void tick(TimeVal now, double delta) {
		base.tick(now, delta);
		detect_collision();
		move(get_x(), get_y() + yspeed * delta);
		check_screen_visibility();
	}

	private void detect_collision() {
		var y = get_y();
		var x = get_x();
		var ry = rocket.get_ypos();
		var rx = rocket.get_xpos();
		var rw = rocket.get_width();
		if(y+get_height()>=ry && y<=ry+rocket.get_height() &&
			x>rx-get_width() && x<rx+rw) {
				explode();
				rocket.explode();
		}
	}

	public void explode() {
		remove_entity();
	}

	private void check_screen_visibility() {
		if (get_y()>gcn.get_scene_height()) {
			remove_entity();
		}
	}
}


class BackgroundEntity : SESpriteEntity
{
	double yspeed = 50.0;
	int inity = 0;

	public static BackgroundEntity create(int starty) {
		var b = new BackgroundEntity();
		b.inity = starty;
		return(b);
	}

	public void initialize(SEResourceCache src) {
		base.initialize(src);
		src.prepare_image("purple_space", "purple_space", get_scene_width(), get_scene_height());
		set_image(SEImage.for_resource("purple_space"));
		move(0, inity);
	}

	public void tick(TimeVal now, double delta) {
		base.tick(now, delta);
		move(get_x(), get_y() + yspeed * delta);
		check_screen_visibility();
	}

	public void check_screen_visibility() {
		if (get_y()>get_scene_height()) {
			move(0, 0-get_scene_height());
		}
	}
}