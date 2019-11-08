void ()firehail_think = {
	if (time < self.splash_time) {
		particle2 ( self.origin, '-30.00000 -30.00000 50.00000', '30.00000 30.00000 100.00000', 140.00000, 16, 10.00000);
		if (self.skin > 1) {
			self.skin = 0;
		} else {
			self.skin += 1;
		}
		if (self.origin_z < self.auraV) {
			AdvanceThinkTime(self, 0.02);
			self.think = balloffire_crash;
		} else {
			AdvanceThinkTime(self, 0.2);
			self.think = firehail_think;
		}
	} else {
		remove(self);
	}
};

void ()firehail_touch =
{
	if (self.controller.origin != VEC_ORIGIN)
		self.controller.cnt -= 1;
	
	balloffire_crash();
};

void ()firehail = {
	local vector org;
	local float i;
	
	org = (self.origin + (random('-150 -150 -15', '150 150 0') * self.spellradiusmod));
	while ((pointcontents(org) != CONTENT_EMPTY) && (i < 4))
	{
		org = (self.origin + (random('-150 -150 -15', '150 150 0') * self.spellradiusmod));
		i += 1;
	}
	
	if (i < 4)
	{
		newmis = spawn();
		newmis.spelldamage = self.spelldamage;
		newmis.spellradiusmod = self.spellradiusmod;
		newmis.owner = self.owner;
		newmis.controller = self.controller;
		newmis.classname = "swelterment";
		setorigin(newmis, org);
		traceline (newmis.origin , (newmis.origin-('0 0 600')) , TRUE , self);
		if (random() < 0.75000) {
			sound ( newmis, CHAN_AUTO, "trail1.wav", 1.00000, ATTN_NORM);
		} else {
			sound ( newmis, CHAN_AUTO, "trail.wav", 1.00000, ATTN_NORM);
		}
		newmis.auraV = (trace_endpos_z - 64.00000);
		setmodel(newmis, "models/fball1.mdl");
		newmis.scale = (1.00000 * newmis.spellradiusmod);
		setsize(newmis, '0 0 0', '0 0 0');
		newmis.cnt = random(3, 10);
		newmis.lifetime = 5.00000;
		newmis.splash_time = (time + newmis.lifetime);
		newmis.solid = SOLID_BBOX;
		newmis.movetype = MOVETYPE_FLYMISSILE;
		//newmis.drawflags = MLS_ABSLIGHT;
		//newmis.drawflags |= (MLS_ABSLIGHT | MLS_FIREFLICKER);
		//newmis.effects = EF_DIMLIGHT;
		newmis.drawflags |= MLS_TORCH;
		newmis.effects = EF_DIMLIGHT;
		newmis.abslight = 1;
		
		newmis.velocity = random('-30.00000 -30.00000 -190.00000', '30.00000 30.00000 -490.00000');
		newmis.angles = vectoangles(newmis.velocity); 
		newmis.touch = firehail_touch;
		AdvanceThinkTime(newmis, 0.02);
		newmis.think = firehail_think;
	}
};

void() inferno_melee =
{
	local entity found;
	found = T_RadiusDamageFlat (self, self.owner, (self.spelldamage + random(self.spelldamage*(-0.12500), self.spelldamage*0.12500))*0.12500, 800.00000 * self.spellradiusmod, self.owner, TRUE);
	while(found)
	{
		particle2 (random(found.absmin, found.absmax), '-30.00000 -30.00000 50.00000', '30.00000 30.00000 100.00000', 140.00000, 16, 10.00000);
		found = found.chain2;
	}
};

void ()obj_redM4 = {
	local entity cloudspawner;
	
	if ( (deathmatch || coop) )
	{
		cloudspawner = find(world, classname, "inferno");
		while ( cloudspawner ) {
			if ((cloudspawner != world) && (cloudspawner.owner == self.owner))
				cloudspawner.splash_time = time;
			
			cloudspawner = find ( cloudspawner, classname, "inferno");
		}
	}
	
	cloudspawner = spawn();
	cloudspawner.classname = "inferno";
	cloudspawner.cloud_style = 2;
	cloudspawner.cloud_height = 500;
	cloudspawner.melee_rate_low = 0.18125;
	cloudspawner.melee_rate_high = 0.36250;
	cloudspawner.th_melee = inferno_melee;
	cloudspawner.missile_rate_low = 0.6375;
	cloudspawner.missile_rate_high = 2;
	cloudspawner.missile_count = rint(4.00000 * self.spellradiusmod);
	cloudspawner.th_missile = firehail;
	clouds_spawner(cloudspawner);
};


//		particle2 ( (self.origin + random('-30 -30 0', '30 30 300')), '-30.00000 -30.00000 50.00000', '30.00000 30.00000 100.00000', random(96, 104), 15, 80.00000);
