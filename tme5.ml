import math

class Robot:
    def __init__(self, x, y, orientation, v_gauche, v_droite):
        if v_gauche == 0 or v_droite == 0:
            raise ValueError("Les vitesses des roues ne peuvent pas être nulles.")
        
        self.x = x
        self.y = y
        self.orientation = orientation
        self.v_gauche = v_gauche
        self.v_droite = v_droite
    
    def avancer(self, duree):
        # Calcul de la distance parcourue en fonction de la durée et des vitesses des roues
        distance = (self.v_gauche + self.v_droite) / 2 * duree
        rad = math.radians(self.orientation)
        # Mise à jour des coordonnées en fonction de l'orientation du robot
        self.x += round(distance * math.cos(rad))
        self.y += round(distance * math.sin(rad))
        print(f"Avance à ({self.x}, {self.y})")
    
    def tourner(self, angle):
        # Si on veut tourner de 90°, appliquer des vitesses opposées
        if angle == 90:
            duree_rotation = 1  # Durée à ajuster selon les besoins
            self.v_gauche = 5
            self.v_droite = -5
            self.avancer(duree_rotation)  # Simuler la rotation
            self.orientation = (self.orientation + 90) % 360  # Ajuster l'orientation
            print(f"Tourne à {self.orientation}°")
    
    def tracer_carre(self, cote):
        if cote <= 0:
            raise ValueError("La longueur du côté du carré doit être positive.")
        
        for _ in range(4):
            somme_vitesses = self.v_gauche + self.v_droite
            if somme_vitesses == 0:
                raise ValueError("La somme des vitesses des roues ne peut pas être nulle.")
            
            # Calcul de la durée nécessaire pour avancer d'un côté du carré
            duree_avance = cote / somme_vitesses
            self.avancer(duree_avance)
            self.tourner(90)

# Simulation
if __name__ == "__main__":
    try:
        robot = Robot(0, 0, 0, 10, 10)  # Initialisation du robot avec des vitesses égales
        print("Début de la simulation...")
        robot.tracer_carre(10)  # Tracer un carré de côté 10 unités
        print("Simulation terminée.")
    except ValueError as e:
        print(f"Erreur : {e}")
